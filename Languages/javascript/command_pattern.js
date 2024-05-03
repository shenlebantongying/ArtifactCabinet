var mgr = {
  func1: function (func_id, s) {
    return func_id + " -> " + s;
  },
  func2: function (func_id, s) {
    return func_id + " -> " + s;
  },
};

mgr.execute = function (name) {
  return mgr[name].apply(
    mgr,
    Array.from(arguments),
  );
};

console.log(mgr.execute("func1", "str1"));
console.log(mgr.execute("func2", "str2"));
