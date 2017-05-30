import * as P from 'Parsimmon';
import * as T from './PTTypes';

function spaced<X>(parser: P.Parser<X>): P.Parser<X> {
  return P.optWhitespace.then(parser).skip(P.optWhitespace);
}

const dice = P.lazy(function () { return _dice });

const num =
  spaced(
    P.digits
      .map(Number)
      .desc('number'));
const flat =
  spaced(
    num
      .map((n) => ({ t: 'Flat', val: n }))
      .desc('Flat'));
const expr =
  spaced(
    P.seq(spaced(num).skip(spaced(P.string('d'))), spaced(num))
      .map(([num, size]): T.Dice => ({ t: "Expr", num, size }))
      .desc('Expr'));
const plus: P.Parser<T.Dice> =
  spaced(
    P.seq(spaced(expr).skip(spaced(P.string('+'))), spaced(dice))
      .map(([left, right]): T.Dice => ({ t: "Plus", left, right }))
      .desc('Plus'));
const bestof: P.Parser<T.Dice> =
  spaced(
    P.seq(spaced(P.string("BestOf")), spaced(P.string("(")), spaced(num), spaced(P.string(",")), spaced(dice), spaced(P.string(")")))
      .map(([_, _2, num, _3, dice]): T.Dice => ({ t: "BestOf", num, dice }))
      .desc("BestOf"));

const _dice = P.alt(bestof, plus, expr, flat);

export function parse(input: string): T.Dice {
  return dice.tryParse(input);
}
