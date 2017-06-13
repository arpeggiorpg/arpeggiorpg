import * as D from '../Dice';

// let describe: any;
// let it: any;
// let expect: any;

describe("dice parsing", () => {
  it("parses a number", () => {
    expect(D.parse("5"))
      .toEqual({ t: 'Flat', val: 5 });
  });
  it("parses an expr", () => {
    expect(D.parse("1d20"))
      .toEqual({ t: "Expr", num: 1, size: 20 });
  });
  it("parses a plus", () => {
    expect(D.parse("1d20+5"))
      .toEqual({ t: "Plus", left: { t: "Expr", num: 1, size: 20 }, right: { t: "Flat", val: 5 } });
  });
  it("parses a plus of two expressions", () => {
    expect(D.parse("1d20+5d8"))
      .toEqual({
        t: "Plus",
        left: { t: "Expr", num: 1, size: 20 },
        right: { t: "Expr", num: 5, size: 8 },
      });
  });
  it("parses a minus", () => {
    expect(D.parse("1d20-2"))
      .toEqual({ t: "Plus", left: { t: "Expr", num: 1, size: 20 }, right: { t: "Flat", val: -2 } });
  });
  // it("parses a plus with the flat on the left", () => {
  //   expect(D.parse("1+1d20"))
  //     .toEqual({ t: "Plus", left: { t: "Flat", val: 1 }, right: { t: "Expr", num: 1, size: 20 } });
  // });
  it("parses a BestOf", () => {
    expect(D.parse("BestOf(5, 1d20)"))
      .toEqual({ t: "BestOf", num: 5, dice: { t: "Expr", num: 1, size: 20 } });
  });

  it("ignores whitespace", () => {
    expect(D.parse(" BestOf ( 5 ,1 d 20 ) "))
      .toEqual({ t: "BestOf", num: 5, dice: { t: "Expr", num: 1, size: 20 } });
  });
});


describe("Dice formatting", () => {
  it("formats a Flat", () => {
    expect(D.format({ t: "Flat", val: 50 }))
      .toEqual("50");
  });
  it("formats an Expr", () => {
    expect(D.format({ t: "Expr", num: 50, size: 100 }))
      .toEqual("50d100");
  });
  it("formats a Plus", () => {
    expect(D.format({ t: "Plus", left: { t: "Expr", num: 50, size: 100 }, right: { t: "Flat", val: 5 } }))
      .toEqual("50d100+5");
  });
  it("formats a negative Plus", () => {
    expect(D.format({ t: "Plus", left: { t: "Expr", num: 50, size: 100 }, right: { t: "Flat", val: -5 } }))
      .toEqual("50d100-5");
  })
  it("formats a BestOf", () => {
    expect(D.format({ t: "BestOf", num: 20, dice: { t: "Expr", num: 50, size: 100 } }))
      .toEqual("BestOf(20, 50d100)");
  })
});
