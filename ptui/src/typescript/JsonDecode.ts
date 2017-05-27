/// A JSON-decoding library inspired by Elm's Json.Decode.
/// (actually, mostly this is really a JavaScript object validation library)

type JSObject = { [index: string]: any };

export function map<T, R>(f: (k: T) => R, d: Decoder<T>): Decoder<R> {
  return function(x: any): R {
    return f(d(x));
  }
}

export function map2<A, B, R>(f: (a: A, b: B) => R, da: Decoder<A>, db: Decoder<B>): Decoder<R> {
  return function(x: any): R {
    return f(da(x), db(x));
  }
}

export function map3<A, B, C, R>(f: (a: A, b: B, c: C) => R, d1: Decoder<A>, d2: Decoder<B>, d3: Decoder<C>): Decoder<R> {
  return function(x: any): R {
    return f(d1(x), d2(x), d3(x));
  }
}

export function map4<A, B, C, D, R>(f: (a: A, b: B, c: C, d: D) => R, d1: Decoder<A>, d2: Decoder<B>, d3: Decoder<C>, d4: Decoder<D>): Decoder<R> {
  return function(x: any): R {
    return f(d1(x), d2(x), d3(x), d4(x));
  }
}

export type Decoder<T> = (i: any) => T;

export function bool(i: any): boolean {
  if (typeof i === "boolean") {
    return i as boolean;
  }
  throw new Error(`Expected boolean, got ${i}.`);
}

export function string(i: any): string {
  if (typeof i === "string") { return i as string; }
  throw new Error(`Expected string, got ${i}.`);
}

export function number(i: any): number {
  if (typeof i === "number") { return i as number; }
  throw new Error(`Expected number, got ${i}.`);
}

export function field<T>(field: string, decoder: Decoder<T>): Decoder<T> {
  return function (obj: any) {
    if (obj.hasOwnProperty(field)) {
      return decoder(obj[field]);
    }
    throw new Error(`Expected ${field} in ${obj}`);
  }
}

export function index<T>(idx: number, decoder: Decoder<T>): Decoder<T> {
  return function (arr: any) {
    if (arr instanceof Array) {
      if (idx in arr) {
        return decoder(arr[idx]);
      }
      throw new Error(`Expected index ${idx} in array ${arr}.`)
    }
    throw new Error(`Expected array, got ${arr}.`)
  }
}

export function array<T>(decoder: Decoder<T>): Decoder<Array<T>> {
  return function (arr: any) {
    let result = [];
    if (!(arr instanceof Array)) {
      throw Error('Expected array, got ${arr}.');
    }
    for (let i in arr) {
      result.push(decoder(arr[i]));
    }
    return result;
  }
}

export function sum<T>(
  name: string,
  nullaryDecoders: { [index: string]: T },
  decoders: { [index: string]: Decoder<T> }): Decoder<T> {
  /// This decoder is specific to the Serde-serialized JSON format:
  /// Nullary variants are just strings like "VariantName"
  /// Unary variants are {"VariantName": value}
  /// "tuple" variants are {"VariantName": [values, ...]}
  /// record variants are {"VariantName": {...}}
  return function (obj: any) {
    for (let variant in nullaryDecoders) {
      if (obj === variant) {
        return nullaryDecoders[variant];
      }
    }
    if (typeof (obj) === 'object') {
      for (let variant in decoders) {
        if (obj.hasOwnProperty(variant)) {
          let decoder = decoders[variant];
          return decoder(obj[variant]);
        }
      }
    }
    console.log(`${name}`, obj);
    throw new Error(`Couldn't decode a ${name} from ${obj}.`);
  }
}
