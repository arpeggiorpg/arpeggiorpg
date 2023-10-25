import { describe, expect, test } from "vitest";

import * as D from "../Dice";

describe("dice parsing", () => {
  test("parses a number", () => {
    expect(D.parse("5"))
      .toEqual({ Flat: { value: 5 } });
  });
  test("parses an expr", () => {
    expect(D.parse("1d20"))
      .toEqual({ Expr: { num: 1, size: 20 } });
  });
  test("parses a plus", () => {
    expect(D.parse("1d20+5"))
      .toEqual({ Plus: [{ Expr: { num: 1, size: 20 } }, { Flat: { value: 5 } }] });
  });
  test("parses a plus of two expressions", () => {
    expect(D.parse("1d20+5d8"))
      .toEqual({
        Plus: [
          { Expr: { num: 1, size: 20 } },
          { Expr: { num: 5, size: 8 } },
        ],
      });
  });
  test("parses a minus", () => {
    expect(D.parse("1d20-2"))
      .toEqual({ Plus: [{ Expr: { num: 1, size: 20 } }, { Flat: { value: -2 } }] });
  });
  // test("parses a plus with the flat on the left", () => {
  //   expect(D.parse("1+1d20"))
  //     .toEqual({ t: "Plus", left: { t: "Flat", val: 1 }, right: { t: "Expr", num: 1, size: 20 } });
  // });
  test("parses a BestOf", () => {
    expect(D.parse("BestOf(5, 1d20)"))
      .toEqual({ BestOf: [5, { Expr: { num: 1, size: 20 } }] });
  });

  test("ignores whitespace", () => {
    expect(D.parse(" BestOf ( 5 ,1 d 20 ) "))
      .toEqual({ BestOf: [5, { Expr: { num: 1, size: 20 } }] });
  });
});

describe("Dice formatting", () => {
  test("formats a Flat", () => {
    expect(D.format({ Flat: { value: 50 } }))
      .toEqual("50");
  });
  test("formats an Expr", () => {
    expect(D.format({ Expr: { num: 50, size: 100 } }))
      .toEqual("50d100");
  });
  test("formats a Plus", () => {
    expect(D.format({ Plus: [{ Expr: { num: 50, size: 100 } }, { Flat: { value: 5 } }] }))
      .toEqual("50d100+5");
  });
  test("formats a negative Plus", () => {
    expect(D.format({ Plus: [{ Expr: { num: 50, size: 100 } }, { Flat: { value: -5 } }] }))
      .toEqual("50d100-5");
  });
  test("formats a BestOf", () => {
    expect(D.format({ BestOf: [20, { Expr: { num: 50, size: 100 } }] }))
      .toEqual("BestOf(20, 50d100)");
  });
});
