use jwt_compact::alg::{RsaPublicKey, StrongKey};
use reqwest;
use serde;

use std::collections::btree_map::Range;
use std::collections::BTreeMap;
use std::ops::{
    Bound,
    Bound::{Included, Unbounded},
};
use std::time::{Duration};
use instant::Instant;

use crate::error::Error;
use crate::token::IdInfo;

pub struct Client {
    pub audiences: Vec<String>,
    pub hosted_domains: Vec<String>,
}

#[derive(Debug, Clone, Deserialize)]
struct CertsObject {
    keys: Vec<JsonCert>,
}

#[derive(Debug, Clone, Deserialize)]
struct JsonCert {
    kid: String,
    e: String,
    kty: String,
    alg: String,
    n: String,
    r#use: String,
}

type Key = String;

#[derive(Clone)]
pub struct CachedCerts {
    keys: BTreeMap<Key, StrongKey<RsaPublicKey>>,
    pub expiry: Option<Instant>,
}

impl CachedCerts {
    pub fn new() -> Self {
        Self {
            keys: BTreeMap::new(),
            expiry: None,
        }
    }

    fn certs_url() -> &'static str {
        "https://www.googleapis.com/oauth2/v2/certs"
    }

    fn get_range<'a>(&'a self, kid: &Option<String>) -> Result<Range<'a, Key, StrongKey<RsaPublicKey>>, Error> {
        match kid {
            None => Ok(self
                .keys
                .range::<String, (Bound<&String>, Bound<&String>)>((Unbounded, Unbounded))),
            Some(kid) => {
                if !self.keys.contains_key(kid) {
                    return Err(Error::InvalidKey);
                }
                Ok(self
                    .keys
                    .range::<String, (Bound<&String>, Bound<&String>)>((
                        Included(kid),
                        Included(kid),
                    )))
            }
        }
    }

    /// Downloads the public Google certificates if it didn't do so already, or based on expiry of
    /// their Cache-Control. Returns `true` if the certificates were updated.
    pub async fn refresh_if_needed(&mut self) -> Result<bool, Error> {
        let check = match self.expiry {
            None => true,
            Some(expiry) => expiry <= Instant::now(),
        };

        if !check {
            return Ok(false);
        }

        let client = Client::new();
        let certs: CertsObject = client.get_any(Self::certs_url(), &mut self.expiry).await?;

        self.keys = BTreeMap::new();

        for cert in certs.keys {
            // We have base64-encoded N and E components; jwt_compact wants
            // BigUInts.
            use base64::{engine::general_purpose::URL_SAFE, Engine};
            let n = URL_SAFE.decode(&cert.n).expect("parsing cert.n");
            let e = URL_SAFE.decode(&cert.e).expect("parsing cert.e");
            let key = jwt_compact::alg::RsaPublicKey::new(num_bigint::BigUint::from_bytes_be(&n), num_bigint::BigUint::from_bytes_be(&e)).expect("constructing RsaPublicKey");
            let key = jwt_compact::alg::StrongKey::try_from(key).expect("constructing StrongKey");

            self.keys.insert(cert.kid.clone(), key);
        }

        Ok(true)
    }
}


impl Client {
    pub fn new() -> Client {
        Client {
            audiences: vec![],
            hosted_domains: vec![],
        }
    }

    /// Verifies that the token is signed by Google's OAuth cerificate,
    /// and check that it has a valid issuer, audience, and hosted domain.
    ///
    /// Returns an error if the client has no configured audiences.
    pub async fn verify(
        &self,
        id_token: &str,
        cached_certs: &CachedCerts,
    ) -> Result<jwt_compact::Claims<IdInfo>, Error> {

        // RADIX TODO: obviously this function needs to be returning Errors instead of panicking
        use jwt_compact::prelude::*;
        let untrusted_token = UntrustedToken::new(&id_token)?;

        for (_, key) in cached_certs.get_range(&untrusted_token.header().key_id)? {
            // Check each certificate

            // let mut validation = Validation::new(Algorithm::RS256);
            let alg = jwt_compact::alg::StrongAlg(jwt_compact::alg::Rsa::rs256());

            // jwt_compact doesn't have explicit support for validating audience. But I'm not sure
            // why google-signin-rs was doing this anyway, because it also verifies the audience in
            // IdInfo.verify.

            // validation.set_audience(&self.audiences);

            let validator = alg.validator(&key);
            let token_data: Token<IdInfo> = validator.validate(&untrusted_token).map_err(|e| Error::InvalidToken)?;
            // let token_data = jsonwebtoken::decode::<IdInfo>(
            //     &id_token,
            //     &DecodingKey::from_rsa_components(&cert.n, &cert.e),
            //     &validation,
            // )?;

            let claims = token_data.claims();
            claims.validate_expiration(&jwt_compact::TimeOptions::default()).map_err(|e| Error::ExpiredToken)?;
            claims.custom.verify(self)?;

            return Ok(claims.clone());
        }

        Err(Error::InvalidToken)
    }

    /// Checks the token using Google's slow OAuth-like authentication flow.
    ///
    /// This checks that the token is signed using Google's OAuth certificate,
    /// but does not check the issuer, audience, or other application-specific verifications.
    ///
    /// This is NOT the recommended way to use the library, but can be used in combination with
    /// [IdInfo.verify](https://docs.rs/google-signin/latest/google_signin/struct.IdInfo.html#impl)
    /// for applications with more complex error-handling requirements.
    pub async fn get_slow_unverified(
        &self,
        id_token: &str,
    ) -> Result<IdInfo<String>, Error> {
        self.get_any(
            &format!(
                "https://www.googleapis.com/oauth2/v3/tokeninfo?id_token={}",
                id_token
            ),
            &mut None,
        )
        .await
    }

    async fn get_any<T: serde::de::DeserializeOwned>(
        &self,
        url: &str,
        cache: &mut Option<Instant>,
    ) -> Result<T, Error> {
        let response = reqwest::get(url).await?;

        let status = response.status().as_u16();
        match status {
            200..=299 => {}
            _ => {
                return Err(Error::InvalidToken);
            }
        }

        if let Some(value) = response.headers().get("Cache-Control") {
            if let Ok(value) = value.to_str() {
                if let Some(cc) = cache_control::CacheControl::from_value(value) {
                    if let Some(max_age) = cc.max_age {
                        let seconds = max_age.num_seconds();
                        if seconds >= 0 {
                            *cache = Some(Instant::now() + Duration::from_secs(seconds as u64));
                        }
                    }
                }
            }
        }

        Ok(response.json().await?)
    }
}
