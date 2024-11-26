const fs = require('fs');

module.exports = function (content) {
  if (this.cacheable) {
    this.cacheable(true);

    this.addDependency(this.resourcePath);

    const buffer = fs.readFileSync(this.resourcePath);
    const base64 = buffer.toString("base64");

    return `export default Buffer.from("${base64}", "base64");`;
  }
};

module.exports.raw = true;
