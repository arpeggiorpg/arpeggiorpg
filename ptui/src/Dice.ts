import * as P from 'parsimmon';
import * as T from './PTTypes';

function spaced<X>(parser: P.Parser<X>): P.Parser<X> {
  return P.optWhitespace.then(parser).skip(P.optWhitespace);
}

const dicep = P.lazy(() => dicep_);

const digits =
  spaced(
    P.digits
      .map(Number)
      .desc('number'));
const flat =
  spaced(
    digits
      .map(n => ({ t: 'Flat', val: n }))
      .desc('Flat'));
const expr =
  spaced(
    P.seq(spaced(digits).skip(spaced(P.string('d'))), spaced(digits))
      .map(([num, size]): T.Dice => ({ t: "Expr", num, size }))
      .desc('Expr'));

const plus: P.Parser<T.Dice> =
  spaced(
    P.seq(spaced(expr).skip(spaced(P.string('+'))), spaced(dicep))
      .map(([left, right]): T.Dice => ({ t: "Plus", left, right }))
      .desc('Plus'));

const minus: P.Parser<T.Dice> =
  // TODO: the data model doesn't support subtraction *in general* --
  // i.e. 1d20-1d8 is not representable, but we can support 1d20-2 at least.
  spaced(
    P.seq(spaced(expr).skip(spaced(P.string('-'))), spaced(digits))
      .map(([left, num]): T.Dice => ({ t: "Plus", left, right: { t: "Flat", val: -num } }))
      .desc('Minus'));

const sum = P.alt(plus, minus);

const bestof: P.Parser<T.Dice> =
  spaced(
    P.seq(
      spaced(P.string("BestOf")), spaced(P.string("(")),
      spaced(digits),
      spaced(P.string(",")), spaced(dicep), spaced(P.string(")")))
      .map(([_, _2, num, _3, dice]): T.Dice => ({ t: "BestOf", num, dice }))
      .desc("BestOf"));

const dicep_ = P.alt(bestof, sum, expr, flat);

export function parse(input: string): T.Dice {
  return dicep.tryParse(input);
}

export function maybeParse(input: string): P.Result<T.Dice> {
  return dicep.parse(input);
}

export function format(d: T.Dice): string {
  switch (d.t) {
    case "Flat": return d.val.toString();
    case "Expr": return (d.num.toString() + "d" + d.size.toString());
    case "Plus":
      const def = format(d.left) + "+" + format(d.right);
      switch (d.right.t) {
        case "Flat":
          if (d.right.val < 0) {
            return (format(d.left) + "-" + (-d.right.val));
          } else {
            return def;
          }
        default: return def;
      }
    case "BestOf":
      return "BestOf(" + d.num.toString() + ", " + format(d.dice) + ")";
  }
}
