import P from 'parsimmon';
import * as T from './PTTypes';

function spaced<X>(parser: P.Parser<X>): P.Parser<X> {
  return P.optWhitespace.then(parser).skip(P.optWhitespace);
}

const dicep = P.lazy(() => dicep_);

const digits: P.Parser<number> =
  spaced(
    P.digits
      .map(Number)
      .desc('number'));
const flat: P.Parser<T.Dice> =
  spaced(
    digits
      .map(value => ({ Flat: { value } }))
      .desc('Flat'));
const expr: P.Parser<T.Dice> =
  spaced(
    P.seq(spaced(digits).skip(spaced(P.string('d'))), spaced(digits))
      .map(([num, size]): T.Dice => ({ Expr: { num, size } }))
      .desc('Expr'));

const plus: P.Parser<T.Dice> =
  spaced(
    P.seq(spaced(expr).skip(spaced(P.string('+'))), spaced(dicep))
      .map(([left, right]): T.Dice => ({ Plus: [left, right] }))
      .desc('Plus'));

const minus: P.Parser<T.Dice> =
  // TODO: the data model doesn't support subtraction *in general* --
  // i.e. 1d20-1d8 is not representable, but we can support 1d20-2 at least.
  spaced(
    P.seq(spaced(expr).skip(spaced(P.string('-'))), spaced(digits))
      .map(([left, num]): T.Dice => ({ Plus: [left, { Flat: { value: -num } }] }))
      .desc('Minus'));

const sum = P.alt(plus, minus);

const bestof: P.Parser<T.Dice> =
  spaced(
    P.seq(
      spaced(P.string("BestOf")), spaced(P.string("(")),
      spaced(digits),
      spaced(P.string(",")), spaced(dicep), spaced(P.string(")")))
      .map(([_, _2, num, _3, dice]): T.Dice => ({ BestOf: [num, dice] }))
      .desc("BestOf"));

const dicep_ = P.alt(bestof, sum, expr, flat);

export function parse(input: string): T.Dice {
  return dicep.tryParse(input);
}

export function maybeParse(input: string): P.Result<T.Dice> {
  return dicep.parse(input);
}

export function format(d: T.Dice): string {
  if ("Flat" in d) return d.Flat.toString();
  if ("Expr" in d) return (d.Expr.num.toString() + "d" + d.Expr.size.toString());
  if ("Plus" in d) {
    const [left, right] = d.Plus;
    if ("Flat" in right && right.Flat.value < 0) {
      return (format(left) + "-" + (-right.Flat));
    } else {
      return format(d.Plus[0]) + "+" + format(d.Plus[1]);;
    }
  }
  if ("BestOf" in d) {
    const [num, dice] = d.BestOf;
    return "BestOf(" + num.toString() + ", " + format(dice) + ")";
  }
  // Typescript is insufficiently smart to know that I've already exhaustively
  // matched all variants of `d` above
  return `Unknown Dice ${JSON.stringify(d)}`;
}
