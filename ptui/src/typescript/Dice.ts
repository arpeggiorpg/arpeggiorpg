import * as P from 'Parsimmon';
import * as T from './PTTypes';

const LNumber = P.regexp(/[0-9]+/).map(Number).desc('number');

export function parse(input: string): T.Dice {
  return {t: 'Flat', val: LNumber.tryParse(input)}
}
