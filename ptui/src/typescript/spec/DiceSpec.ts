import * as D from '../Dice';

let describe: any;
let it: any;
let expect: any;

describe("dice parsing", function () {
  it("parses a number", function () {
    expect(D.parse("5")).toEqual({ t: 'Flat', val: 5 })
  })
});
