"use strict";
var _radix$pandt$Native_NativePT = function () {
  function parseDice(input) {
    console.log("[parseDice]", input);
    try {
      var result = PTDice.parse(input);
      console.log("[parseDice:return]", result);
      return result;
    } catch (e) {
      console.log("[parseDice:error]");
      return undefined;
    }
  }
  function formatDice(dice) {
    console.log("[formatDice]", dice);
    var result = PTDice.format(dice);
    console.log("[formatDice:return]", result);
    return result;
  }
  return {
    parseDice: parseDice,
    formatDice: formatDice
  };
}();
