/// A JSON-decoding library inspired by Elm's Json.Decode.
/// (actually, mostly this is really a JavaScript object validation library)

type JSObject = { [index: string]: any };

export function bool(i: any): boolean {
  if (typeof i == "boolean") {
    return i as boolean;
  }
  throw new Error(`Not a boolean: ${i}.`);
}

export function string(i: any): string {
  if (typeof i == "string") { return i as string; }
  throw new Error(`Not a string: ${i}.`);
}

export function number(i: any): number {
  if (typeof i == "number") { return i as number; }
  throw new Error(`Not a number: ${i}.`);
}

type Decoder<T> = (i: any) => T;

export function field<T>(obj: JSObject, field: string, decoder: Decoder<T>): T {
  if (obj.hasOwnProperty(field)) {
    return decoder(obj[field]);
  }
  throw new Error(`Expected ${field} in ${obj}`);
}

export function index<T>(arr: any, idx: number, decoder: Decoder<T>): T {
  if (arr instanceof Array) {
    if (idx in arr) {
      return decoder(arr[idx]);
    }
    throw new Error(`Expected index ${idx} in array ${arr}.`)
  }
  throw new Error(`Expected an array: ${arr}.`)
}

export function sum<T>(name: string, obj: any, nullaryDecoders: { [index: string]: T }, decoders: { [index: string]: Decoder<T> }): T {
  /// This decoder is specific to the Serde-serialized JSON format:
  /// Nullary variants are just strings like "VariantName"
  /// Unary variants are {"VariantName": value}
  /// "tuple" variants are {"VariantName": [values, ...]}
  /// record variants are {"VariantName": {...}}
  for (var variant in nullaryDecoders) {
    if (obj === variant) {
      return nullaryDecoders[variant];
    }
  }
  if (typeof (obj) == 'object') {
    for (var variant in decoders) {
      if (obj.hasOwnProperty(variant)) {
        let decoder = decoders[variant];
        return decoder(obj[variant]);
      }
    }
  }
  throw new Error(`Couldn't decode a ${name} from ${obj}.`);
}
