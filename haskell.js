/**
 * Haskell 
 */
module.exports = {
  // find document blocks between '###' and '###'
  docBlocksRegExp: /{-?(.+?)\uffff?(?:\s*)?-}/g,
  // remove not needed tabs at the beginning
  inlineRegExp: /^(\t*)?/gm
};  