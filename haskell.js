/**
 * Haskell comments detection for ApiDoc 
 */
module.exports = {
  docBlocksRegExp: /{-?(.+?)\uffff?(?:\s*)?-}/g,
  inlineRegExp: /^(\t*)?/gm
};  