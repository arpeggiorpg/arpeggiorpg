import * as D from '../Dice';

// let describe: any;
// let it: any;
// let expect: any;

describe("dice parsing", function () {
  it("parses a number", function () {
    expect(D.parse("5"))
      .toEqual({ t: 'Flat', val: 5 });
  });
  it("parses an expr", function () {
    expect(D.parse("1d20"))
      .toEqual({ t: "Expr", num: 1, size: 20 });
  });
  it("parses a plus", function () {
    expect(D.parse("1d20+5"))
      .toEqual({ t: "Plus", left: { t: "Expr", num: 1, size: 20 }, right: { t: "Flat", val: 5 } });
  });
  it("parses a plus of two expressions", function () {
    expect(D.parse("1d20+5d8"))
      .toEqual({
        t: "Plus",
        left: { t: "Expr", num: 1, size: 20 },
        right: { t: "Expr", num: 5, size: 8 }
      });
  });
  // it("parses a plus with the flat on the left", function () {
  //   expect(D.parse("1+1d20"))
  //     .toEqual({ t: "Plus", left: { t: "Flat", val: 1 }, right: { t: "Expr", num: 1, size: 20 } });
  // });
  it("parses a BestOf", function () {
    expect(D.parse("BestOf(5, 1d20)"))
      .toEqual({ t: "BestOf", num: 5, dice: { t: "Expr", num: 1, size: 20 } });
  });

  it("ignores whitespace", function() {
    expect(D.parse(" BestOf ( 5 ,1 d 20 ) "))
      .toEqual({ t: "BestOf", num: 5, dice: { t: "Expr", num: 1, size: 20 } });

  });


});
