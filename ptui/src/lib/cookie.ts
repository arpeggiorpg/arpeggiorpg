export function getCookie(name: string): string | undefined {
  let cookies = document.cookie.split(';');
  for (let cookie of cookies) {
    cookie = cookie.trimStart();
    if (cookie.startsWith(`${name}=`)) {
      return cookie.split('=', 2)[1];
    }
  }
}

if (window) {
  (window as any).ptGetCookie = getCookie;
}
