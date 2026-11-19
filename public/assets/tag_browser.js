// build/dev/javascript/prelude.mjs
class CustomType {
  withFields(fields) {
    let properties = Object.keys(this).map((label) => (label in fields) ? fields[label] : this[label]);
    return new this.constructor(...properties);
  }
}

class List {
  static fromArray(array, tail) {
    return toList(array, tail);
  }
  [Symbol.iterator]() {
    return new ListIterator(this);
  }
  toArray() {
    return [...this];
  }
  atLeastLength(desired) {
    let current = this;
    while (desired-- > 0 && current)
      current = current.tail;
    return current !== undefined;
  }
  hasLength(desired) {
    let current = this;
    while (desired-- > 0 && current)
      current = current.tail;
    return desired === -1 && current instanceof Empty;
  }
  countLength() {
    let current = this;
    let length = 0;
    while (current) {
      current = current.tail;
      length++;
    }
    return length - 1;
  }
}
function prepend(element, tail) {
  return new NonEmpty(element, tail);
}
function toList(elements, tail) {
  let t = tail || List$Empty$const;
  for (let i = elements.length - 1;i >= 0; --i) {
    t = new NonEmpty(elements[i], t);
  }
  return t;
}

class ListIterator {
  #current;
  constructor(current) {
    this.#current = current;
  }
  next() {
    if (this.#current instanceof Empty) {
      return { done: true };
    } else {
      let { head, tail } = this.#current;
      this.#current = tail;
      return { value: head, done: false };
    }
  }
}

class Empty extends List {
}
var List$Empty$const = new Empty;
var List$Empty = () => List$Empty$const;
var List$isEmpty = (value) => value instanceof Empty;

class NonEmpty extends List {
  constructor(head, tail) {
    super();
    this.head = head;
    this.tail = tail;
  }
}
var List$NonEmpty = (head, tail) => new NonEmpty(head, tail);
var List$isNonEmpty = (value) => value instanceof NonEmpty;
var List$NonEmpty$first = (value) => value.head;
var List$NonEmpty$rest = (value) => value.tail;

class BitArray {
  bitSize;
  byteSize;
  bitOffset;
  rawBuffer;
  constructor(buffer, bitSize, bitOffset) {
    if (!(buffer instanceof Uint8Array)) {
      throw globalThis.Error("BitArray can only be constructed from a Uint8Array");
    }
    this.bitSize = bitSize ?? buffer.length * 8;
    this.byteSize = Math.trunc((this.bitSize + 7) / 8);
    this.bitOffset = bitOffset ?? 0;
    if (this.bitSize < 0) {
      throw globalThis.Error(`BitArray bit size is invalid: ${this.bitSize}`);
    }
    if (this.bitOffset < 0 || this.bitOffset > 7) {
      throw globalThis.Error(`BitArray bit offset is invalid: ${this.bitOffset}`);
    }
    if (buffer.length !== Math.trunc((this.bitOffset + this.bitSize + 7) / 8)) {
      throw globalThis.Error("BitArray buffer length is invalid");
    }
    this.rawBuffer = buffer;
  }
  byteAt(index) {
    if (index < 0 || index >= this.byteSize) {
      return;
    }
    return bitArrayByteAt(this.rawBuffer, this.bitOffset, index);
  }
  equals(other) {
    if (this.bitSize !== other.bitSize) {
      return false;
    }
    const wholeByteCount = Math.trunc(this.bitSize / 8);
    if (this.bitOffset === 0 && other.bitOffset === 0) {
      for (let i = 0;i < wholeByteCount; i++) {
        if (this.rawBuffer[i] !== other.rawBuffer[i]) {
          return false;
        }
      }
      const trailingBitsCount = this.bitSize % 8;
      if (trailingBitsCount) {
        const unusedLowBitCount = 8 - trailingBitsCount;
        if (this.rawBuffer[wholeByteCount] >> unusedLowBitCount !== other.rawBuffer[wholeByteCount] >> unusedLowBitCount) {
          return false;
        }
      }
    } else {
      for (let i = 0;i < wholeByteCount; i++) {
        const a = bitArrayByteAt(this.rawBuffer, this.bitOffset, i);
        const b = bitArrayByteAt(other.rawBuffer, other.bitOffset, i);
        if (a !== b) {
          return false;
        }
      }
      const trailingBitsCount = this.bitSize % 8;
      if (trailingBitsCount) {
        const a = bitArrayByteAt(this.rawBuffer, this.bitOffset, wholeByteCount);
        const b = bitArrayByteAt(other.rawBuffer, other.bitOffset, wholeByteCount);
        const unusedLowBitCount = 8 - trailingBitsCount;
        if (a >> unusedLowBitCount !== b >> unusedLowBitCount) {
          return false;
        }
      }
    }
    return true;
  }
  get buffer() {
    if (this.bitOffset !== 0 || this.bitSize % 8 !== 0) {
      throw new globalThis.Error("BitArray.buffer does not support unaligned bit arrays");
    }
    return this.rawBuffer;
  }
  get length() {
    if (this.bitOffset !== 0 || this.bitSize % 8 !== 0) {
      throw new globalThis.Error("BitArray.length does not support unaligned bit arrays");
    }
    return this.rawBuffer.length;
  }
}
function bitArrayByteAt(buffer, bitOffset, index) {
  if (bitOffset === 0) {
    return buffer[index] ?? 0;
  } else {
    const a = buffer[index] << bitOffset & 255;
    const b = buffer[index + 1] >> 8 - bitOffset;
    return a | b;
  }
}
class Result extends CustomType {
  static isResult(data) {
    return data instanceof Result;
  }
}

class Ok extends Result {
  constructor(value) {
    super();
    this[0] = value;
  }
  isOk() {
    return true;
  }
}
var Result$Ok = (value) => new Ok(value);
var Result$isOk = (value) => value instanceof Ok;
var Result$Ok$0 = (value) => value[0];

class Error2 extends Result {
  constructor(detail) {
    super();
    this[0] = detail;
  }
  isOk() {
    return false;
  }
}
var Result$Error = (detail) => new Error2(detail);
var Result$isError = (value) => value instanceof Error2;
function isEqual(x, y) {
  let values = [x, y];
  while (values.length) {
    let a = values.pop();
    let b = values.pop();
    if (a === b)
      continue;
    if (!isObject(a) || !isObject(b))
      return false;
    let unequal = !structurallyCompatibleObjects(a, b) || unequalDates(a, b) || unequalBuffers(a, b) || unequalArrays(a, b) || unequalMaps(a, b) || unequalSets(a, b) || unequalRegExps(a, b);
    if (unequal)
      return false;
    const proto = Object.getPrototypeOf(a);
    if (proto !== null && typeof proto.equals === "function") {
      try {
        if (a.equals(b))
          continue;
        else
          return false;
      } catch {}
    }
    let [keys, get] = getters(a);
    const ka = keys(a);
    const kb = keys(b);
    if (ka.length !== kb.length)
      return false;
    for (let k of ka) {
      values.push(get(a, k), get(b, k));
    }
  }
  return true;
}
function getters(object) {
  if (object instanceof Map) {
    return [(x) => x.keys(), (x, y) => x.get(y)];
  } else {
    let extra = object instanceof globalThis.Error ? ["message"] : [];
    return [(x) => [...extra, ...Object.keys(x)], (x, y) => x[y]];
  }
}
function unequalDates(a, b) {
  return a instanceof Date && (a > b || a < b);
}
function unequalBuffers(a, b) {
  return !(a instanceof BitArray) && a.buffer instanceof ArrayBuffer && a.BYTES_PER_ELEMENT && !(a.byteLength === b.byteLength && a.every((n, i) => n === b[i]));
}
function unequalArrays(a, b) {
  return Array.isArray(a) && a.length !== b.length;
}
function unequalMaps(a, b) {
  return a instanceof Map && a.size !== b.size;
}
function unequalSets(a, b) {
  return a instanceof Set && (a.size != b.size || [...a].some((e) => !b.has(e)));
}
function unequalRegExps(a, b) {
  return a instanceof RegExp && (a.source !== b.source || a.flags !== b.flags);
}
function isObject(a) {
  return typeof a === "object" && a !== null;
}
function structurallyCompatibleObjects(a, b) {
  if (typeof a !== "object" && typeof b !== "object" && (!a || !b))
    return false;
  let nonstructural = [Promise, WeakSet, WeakMap, Function];
  if (nonstructural.some((c) => a instanceof c))
    return false;
  return a.constructor === b.constructor;
}
function makeError(variant, file, module, line, fn, message, extra) {
  let error = new globalThis.Error(message);
  error.gleam_error = variant;
  error.file = file;
  error.module = module;
  error.line = line;
  error.function = fn;
  error.fn = fn;
  for (let k in extra)
    error[k] = extra[k];
  return error;
}
// build/dev/javascript/gleam_stdlib/dict.mjs
var referenceMap = /* @__PURE__ */ new WeakMap;
var tempDataView = /* @__PURE__ */ new DataView(/* @__PURE__ */ new ArrayBuffer(8));
var referenceUID = 0;
function hashByReference(o) {
  const known = referenceMap.get(o);
  if (known !== undefined) {
    return known;
  }
  const hash = referenceUID++;
  if (referenceUID === 2147483647) {
    referenceUID = 0;
  }
  referenceMap.set(o, hash);
  return hash;
}
function hashMerge(a, b) {
  return a ^ b + 2654435769 + (a << 6) + (a >> 2) | 0;
}
function hashString(s) {
  let hash = 0;
  const len = s.length;
  for (let i = 0;i < len; i++) {
    hash = Math.imul(31, hash) + s.charCodeAt(i) | 0;
  }
  return hash;
}
function hashNumber(n) {
  tempDataView.setFloat64(0, n);
  const i = tempDataView.getInt32(0);
  const j = tempDataView.getInt32(4);
  return Math.imul(73244475, i >> 16 ^ i) ^ j;
}
function hashBigInt(n) {
  return hashString(n.toString());
}
function hashObject(o) {
  const proto = Object.getPrototypeOf(o);
  if (proto !== null && typeof proto.hashCode === "function") {
    try {
      const code = o.hashCode(o);
      if (typeof code === "number") {
        return code;
      }
    } catch {}
  }
  if (o instanceof Promise || o instanceof WeakSet || o instanceof WeakMap) {
    return hashByReference(o);
  }
  if (o instanceof Date) {
    return hashNumber(o.getTime());
  }
  let h = 0;
  if (o instanceof ArrayBuffer) {
    o = new Uint8Array(o);
  }
  if (Array.isArray(o) || o instanceof Uint8Array) {
    for (let i = 0;i < o.length; i++) {
      h = Math.imul(31, h) + getHash(o[i]) | 0;
    }
  } else if (o instanceof Set) {
    o.forEach((v) => {
      h = h + getHash(v) | 0;
    });
  } else if (o instanceof Map) {
    o.forEach((v, k) => {
      h = h + hashMerge(getHash(v), getHash(k)) | 0;
    });
  } else {
    const keys = Object.keys(o);
    for (let i = 0;i < keys.length; i++) {
      const k = keys[i];
      const v = o[k];
      h = h + hashMerge(getHash(v), hashString(k)) | 0;
    }
  }
  return h;
}
function getHash(u) {
  if (u === null)
    return 1108378658;
  if (u === undefined)
    return 1108378659;
  if (u === true)
    return 1108378657;
  if (u === false)
    return 1108378656;
  switch (typeof u) {
    case "number":
      return hashNumber(u);
    case "string":
      return hashString(u);
    case "bigint":
      return hashBigInt(u);
    case "object":
      return hashObject(u);
    case "symbol":
      return hashByReference(u);
    case "function":
      return hashByReference(u);
    default:
      return 0;
  }
}

class Dict {
  constructor(size, root) {
    this.size = size;
    this.root = root;
  }
}
var bits = 5;
var mask = (1 << bits) - 1;
var noElementMarker = Symbol();

class Node {
  constructor(generation, datamap, nodemap, data) {
    this.datamap = datamap;
    this.nodemap = nodemap;
    this.data = data;
    this.generation = generation;
  }
  equals(other) {
    if (this === other)
      return true;
    if (!(other instanceof Node))
      return false;
    if (this.datamap !== other.datamap || this.nodemap !== other.nodemap) {
      return false;
    }
    const leftData = this.data;
    const rightData = other.data;
    if (leftData.length !== rightData.length)
      return false;
    if (this.datamap === 0 && this.nodemap === 0) {
      return this.#equalsOverflowEntries(rightData);
    }
    const edgesStart = leftData.length - popcount(this.nodemap);
    for (let i = 0;i < edgesStart; i += 2) {
      if (!isEqual(leftData[i], rightData[i]) || !isEqual(leftData[i + 1], rightData[i + 1])) {
        return false;
      }
    }
    for (let i = edgesStart;i < leftData.length; ++i) {
      if (!leftData[i].equals(rightData[i]))
        return false;
    }
    return true;
  }
  #equalsOverflowEntries(otherData) {
    const data = this.data;
    entries:
      for (let i = 0;i < data.length; i += 2) {
        for (let j = 0;j < otherData.length; j += 2) {
          if (isEqual(data[i], otherData[j])) {
            if (!isEqual(data[i + 1], otherData[j + 1]))
              return false;
            continue entries;
          }
        }
        return false;
      }
    return true;
  }
  hashCode() {
    const data = this.data;
    const edgesStart = data.length - popcount(this.nodemap);
    let hash = 0;
    for (let i = 0;i < edgesStart; i += 2) {
      hash = hash + hashMerge(getHash(data[i + 1]), getHash(data[i])) | 0;
    }
    for (let i = edgesStart;i < data.length; ++i) {
      hash = hash + data[i].hashCode() | 0;
    }
    return hash;
  }
}
var emptyNode = /* @__PURE__ */ newNode(0);
var emptyDict = /* @__PURE__ */ new Dict(0, emptyNode);
var errorNil = /* @__PURE__ */ Result$Error(undefined);
function newNode(generation) {
  return new Node(generation, 0, 0, []);
}
function copyNode(node, generation) {
  if (node.generation === generation) {
    return node;
  }
  const newData = node.data.slice(0);
  return new Node(generation, node.datamap, node.nodemap, newData);
}
function copyAndSet(node, generation, idx, val) {
  if (node.data[idx] === val) {
    return node;
  }
  node = copyNode(node, generation);
  node.data[idx] = val;
  return node;
}
function copyAndInsertPair(node, generation, bit, idx, key, val) {
  const data = node.data;
  const length = data.length;
  const newData = new Array(length + 2);
  let readIndex = 0;
  let writeIndex = 0;
  while (readIndex < idx)
    newData[writeIndex++] = data[readIndex++];
  newData[writeIndex++] = key;
  newData[writeIndex++] = val;
  while (readIndex < length)
    newData[writeIndex++] = data[readIndex++];
  return new Node(generation, node.datamap | bit, node.nodemap, newData);
}
function make() {
  return emptyDict;
}
function from(iterable) {
  let transient = toTransient(emptyDict);
  for (const [key, value] of iterable) {
    transient = destructiveTransientInsert(key, value, transient);
  }
  return fromTransient(transient);
}
function get(dict, key) {
  const result = lookup(dict.root, key, getHash(key));
  return result !== noElementMarker ? Result$Ok(result) : errorNil;
}
function lookup(node, key, hash) {
  for (let shift = 0;shift < 32; shift += bits) {
    const data = node.data;
    const bit = hashbit(hash, shift);
    if (node.nodemap & bit) {
      node = data[data.length - 1 - index(node.nodemap, bit)];
    } else if (node.datamap & bit) {
      const dataidx = Math.imul(index(node.datamap, bit), 2);
      return isEqual(key, data[dataidx]) ? data[dataidx + 1] : noElementMarker;
    } else {
      return noElementMarker;
    }
  }
  const overflow = node.data;
  for (let i = 0;i < overflow.length; i += 2) {
    if (isEqual(key, overflow[i])) {
      return overflow[i + 1];
    }
  }
  return noElementMarker;
}
function toTransient(dict) {
  return {
    generation: nextGeneration(dict),
    root: dict.root,
    size: dict.size,
    dict
  };
}
function fromTransient(transient) {
  if (transient.root === transient.dict.root) {
    return transient.dict;
  }
  return new Dict(transient.size, transient.root);
}
function nextGeneration(dict) {
  const root = dict.root;
  if (root.generation < Number.MAX_SAFE_INTEGER) {
    return root.generation + 1;
  }
  const queue = [root];
  while (queue.length) {
    const node = queue.pop();
    node.generation = 0;
    const nodeStart = node.data.length - popcount(node.nodemap);
    for (let i = nodeStart;i < node.data.length; ++i) {
      queue.push(node.data[i]);
    }
  }
  return 1;
}
var globalTransient = /* @__PURE__ */ toTransient(emptyDict);
function insert(dict, key, value) {
  globalTransient.generation = nextGeneration(dict);
  globalTransient.size = dict.size;
  const hash = getHash(key);
  const root = insertIntoNode(globalTransient, dict.root, key, value, hash, 0);
  if (root === dict.root) {
    return dict;
  }
  return new Dict(globalTransient.size, root);
}
function destructiveTransientInsert(key, value, transient) {
  const hash = getHash(key);
  transient.root = insertIntoNode(transient, transient.root, key, value, hash, 0);
  return transient;
}
function insertIntoNode(transient, node, key, value, hash, shift) {
  const data = node.data;
  const generation = transient.generation;
  if (shift > 32) {
    for (let i = 0;i < data.length; i += 2) {
      if (isEqual(key, data[i])) {
        return copyAndSet(node, generation, i + 1, value);
      }
    }
    transient.size += 1;
    return copyAndInsertPair(node, generation, 0, data.length, key, value);
  }
  const bit = hashbit(hash, shift);
  if (node.nodemap & bit) {
    const nodeidx = data.length - 1 - index(node.nodemap, bit);
    let child = data[nodeidx];
    child = insertIntoNode(transient, child, key, value, hash, shift + bits);
    return copyAndSet(node, generation, nodeidx, child);
  }
  const dataidx = Math.imul(index(node.datamap, bit), 2);
  if ((node.datamap & bit) === 0) {
    transient.size += 1;
    return copyAndInsertPair(node, generation, bit, dataidx, key, value);
  }
  if (isEqual(key, data[dataidx])) {
    return copyAndSet(node, generation, dataidx + 1, value);
  }
  const childShift = shift + bits;
  let child = emptyNode;
  child = insertIntoNode(transient, child, key, value, hash, childShift);
  const key2 = data[dataidx];
  const value2 = data[dataidx + 1];
  const hash2 = getHash(key2);
  child = insertIntoNode(transient, child, key2, value2, hash2, childShift);
  transient.size -= 1;
  const length = data.length;
  const nodeidx = length - 1 - index(node.nodemap, bit);
  const newData = new Array(length - 1);
  let readIndex = 0;
  let writeIndex = 0;
  while (readIndex < dataidx)
    newData[writeIndex++] = data[readIndex++];
  readIndex += 2;
  while (readIndex <= nodeidx)
    newData[writeIndex++] = data[readIndex++];
  newData[writeIndex++] = child;
  while (readIndex < length)
    newData[writeIndex++] = data[readIndex++];
  return new Node(generation, node.datamap ^ bit, node.nodemap | bit, newData);
}
function fold(dict, state, fun) {
  const queue = [dict.root];
  while (queue.length) {
    const node = queue.pop();
    const data = node.data;
    const edgesStart = data.length - popcount(node.nodemap);
    for (let i = 0;i < edgesStart; i += 2) {
      state = fun(state, data[i], data[i + 1]);
    }
    for (let i = edgesStart;i < data.length; ++i) {
      queue.push(data[i]);
    }
  }
  return state;
}
function popcount(n) {
  n -= n >>> 1 & 1431655765;
  n = (n & 858993459) + (n >>> 2 & 858993459);
  return Math.imul(n + (n >>> 4) & 252645135, 16843009) >>> 24;
}
function index(bitmap, bit) {
  return popcount(bitmap & bit - 1);
}
function hashbit(hash, shift) {
  return 1 << (hash >>> shift & mask);
}

// build/dev/javascript/gleam_stdlib/gleam/option.mjs
class Some extends CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
class None extends CustomType {
}
var Option$None$const = new None;

// build/dev/javascript/gleam_stdlib/gleam/order.mjs
class Lt extends CustomType {
}
var Order$Lt$const = new Lt;
var Order$Lt = () => Order$Lt$const;
class Eq extends CustomType {
}
var Order$Eq$const = new Eq;
var Order$Eq = () => Order$Eq$const;
class Gt extends CustomType {
}
var Order$Gt$const = new Gt;
var Order$Gt = () => Order$Gt$const;

// build/dev/javascript/gleam_stdlib/gleam/list.mjs
class Ascending extends CustomType {
}
var Sorting$Ascending$const = new Ascending;

class Descending extends CustomType {
}
var Sorting$Descending$const = new Descending;
function length_loop(loop$list, loop$count) {
  while (true) {
    let list = loop$list;
    let count = loop$count;
    if (list instanceof Empty) {
      return count;
    } else {
      let list$1 = list.tail;
      loop$list = list$1;
      loop$count = count + 1;
    }
  }
}
function length(list) {
  return length_loop(list, 0);
}
function reverse_and_prepend(loop$prefix, loop$suffix) {
  while (true) {
    let prefix = loop$prefix;
    let suffix = loop$suffix;
    if (prefix instanceof Empty) {
      return suffix;
    } else {
      let first$1 = prefix.head;
      let rest$1 = prefix.tail;
      loop$prefix = rest$1;
      loop$suffix = prepend(first$1, suffix);
    }
  }
}
function reverse(list) {
  return reverse_and_prepend(list, List$Empty$const);
}
function map_loop(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list instanceof Empty) {
      return reverse(acc);
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      loop$list = rest$1;
      loop$fun = fun;
      loop$acc = prepend(fun(first$1), acc);
    }
  }
}
function map2(list, fun) {
  return map_loop(list, fun, List$Empty$const);
}
function index_map_loop(loop$list, loop$fun, loop$index, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let index = loop$index;
    let acc = loop$acc;
    if (list instanceof Empty) {
      return reverse(acc);
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let acc$1 = prepend(fun(first$1, index), acc);
      loop$list = rest$1;
      loop$fun = fun;
      loop$index = index + 1;
      loop$acc = acc$1;
    }
  }
}
function index_map(list, fun) {
  return index_map_loop(list, fun, 0, List$Empty$const);
}
function append_loop(loop$first, loop$second) {
  while (true) {
    let first = loop$first;
    let second = loop$second;
    if (first instanceof Empty) {
      return second;
    } else {
      let first$1 = first.head;
      let rest$1 = first.tail;
      loop$first = rest$1;
      loop$second = prepend(first$1, second);
    }
  }
}
function append(first, second) {
  return append_loop(reverse(first), second);
}
function fold2(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list instanceof Empty) {
      return initial;
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      loop$list = rest$1;
      loop$initial = fun(initial, first$1);
      loop$fun = fun;
    }
  }
}
function merge_descendings(loop$list1, loop$list2, loop$compare, loop$acc) {
  while (true) {
    let list1 = loop$list1;
    let list2 = loop$list2;
    let compare = loop$compare;
    let acc = loop$acc;
    if (list1 instanceof Empty) {
      let list = list2;
      return reverse_and_prepend(list, acc);
    } else if (list2 instanceof Empty) {
      let list = list1;
      return reverse_and_prepend(list, acc);
    } else {
      let first1 = list1.head;
      let rest1 = list1.tail;
      let first2 = list2.head;
      let rest2 = list2.tail;
      let $ = compare(first1, first2);
      if ($ instanceof Lt) {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare;
        loop$acc = prepend(first2, acc);
      } else if ($ instanceof Eq) {
        loop$list1 = rest1;
        loop$list2 = list2;
        loop$compare = compare;
        loop$acc = prepend(first1, acc);
      } else {
        loop$list1 = rest1;
        loop$list2 = list2;
        loop$compare = compare;
        loop$acc = prepend(first1, acc);
      }
    }
  }
}
function merge_descending_pairs(loop$sequences, loop$compare, loop$acc) {
  while (true) {
    let sequences = loop$sequences;
    let compare = loop$compare;
    let acc = loop$acc;
    if (sequences instanceof Empty) {
      return reverse(acc);
    } else {
      let $ = sequences.tail;
      if ($ instanceof Empty) {
        let sequence = sequences.head;
        return reverse(prepend(reverse(sequence), acc));
      } else {
        let descending1 = sequences.head;
        let descending2 = $.head;
        let rest$1 = $.tail;
        let ascending = merge_descendings(descending1, descending2, compare, List$Empty$const);
        loop$sequences = rest$1;
        loop$compare = compare;
        loop$acc = prepend(ascending, acc);
      }
    }
  }
}
function merge_ascendings(loop$list1, loop$list2, loop$compare, loop$acc) {
  while (true) {
    let list1 = loop$list1;
    let list2 = loop$list2;
    let compare = loop$compare;
    let acc = loop$acc;
    if (list1 instanceof Empty) {
      let list = list2;
      return reverse_and_prepend(list, acc);
    } else if (list2 instanceof Empty) {
      let list = list1;
      return reverse_and_prepend(list, acc);
    } else {
      let first1 = list1.head;
      let rest1 = list1.tail;
      let first2 = list2.head;
      let rest2 = list2.tail;
      let $ = compare(first1, first2);
      if ($ instanceof Lt) {
        loop$list1 = rest1;
        loop$list2 = list2;
        loop$compare = compare;
        loop$acc = prepend(first1, acc);
      } else if ($ instanceof Eq) {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare;
        loop$acc = prepend(first2, acc);
      } else {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare;
        loop$acc = prepend(first2, acc);
      }
    }
  }
}
function merge_ascending_pairs(loop$sequences, loop$compare, loop$acc) {
  while (true) {
    let sequences = loop$sequences;
    let compare = loop$compare;
    let acc = loop$acc;
    if (sequences instanceof Empty) {
      return reverse(acc);
    } else {
      let $ = sequences.tail;
      if ($ instanceof Empty) {
        let sequence = sequences.head;
        return reverse(prepend(reverse(sequence), acc));
      } else {
        let ascending1 = sequences.head;
        let ascending2 = $.head;
        let rest$1 = $.tail;
        let descending = merge_ascendings(ascending1, ascending2, compare, List$Empty$const);
        loop$sequences = rest$1;
        loop$compare = compare;
        loop$acc = prepend(descending, acc);
      }
    }
  }
}
function merge_all(loop$sequences, loop$direction, loop$compare) {
  while (true) {
    let sequences = loop$sequences;
    let direction = loop$direction;
    let compare = loop$compare;
    if (sequences instanceof Empty) {
      return sequences;
    } else if (direction instanceof Ascending) {
      if (sequences.tail instanceof Empty) {
        let sequence = sequences.head;
        return sequence;
      } else {
        let sequences$1 = merge_ascending_pairs(sequences, compare, List$Empty$const);
        loop$sequences = sequences$1;
        loop$direction = Sorting$Descending$const;
        loop$compare = compare;
      }
    } else if (sequences.tail instanceof Empty) {
      let sequence = sequences.head;
      return reverse(sequence);
    } else {
      let sequences$1 = merge_descending_pairs(sequences, compare, List$Empty$const);
      loop$sequences = sequences$1;
      loop$direction = Sorting$Ascending$const;
      loop$compare = compare;
    }
  }
}
function sequences(loop$list, loop$compare, loop$growing, loop$direction, loop$prev, loop$acc) {
  while (true) {
    let list = loop$list;
    let compare = loop$compare;
    let growing = loop$growing;
    let direction = loop$direction;
    let prev = loop$prev;
    let acc = loop$acc;
    let growing$1 = prepend(prev, growing);
    if (list instanceof Empty) {
      if (direction instanceof Ascending) {
        return prepend(reverse(growing$1), acc);
      } else {
        return prepend(growing$1, acc);
      }
    } else {
      let new$1 = list.head;
      let rest$1 = list.tail;
      let $ = compare(prev, new$1);
      if (direction instanceof Ascending) {
        if ($ instanceof Lt) {
          loop$list = rest$1;
          loop$compare = compare;
          loop$growing = growing$1;
          loop$direction = direction;
          loop$prev = new$1;
          loop$acc = acc;
        } else if ($ instanceof Eq) {
          loop$list = rest$1;
          loop$compare = compare;
          loop$growing = growing$1;
          loop$direction = direction;
          loop$prev = new$1;
          loop$acc = acc;
        } else {
          let _block;
          if (direction instanceof Ascending) {
            _block = prepend(reverse(growing$1), acc);
          } else {
            _block = prepend(growing$1, acc);
          }
          let acc$1 = _block;
          if (rest$1 instanceof Empty) {
            return prepend(prepend(new$1, List$Empty$const), acc$1);
          } else {
            let next = rest$1.head;
            let rest$2 = rest$1.tail;
            let _block$1;
            let $1 = compare(new$1, next);
            if ($1 instanceof Lt) {
              _block$1 = Sorting$Ascending$const;
            } else if ($1 instanceof Eq) {
              _block$1 = Sorting$Ascending$const;
            } else {
              _block$1 = Sorting$Descending$const;
            }
            let direction$1 = _block$1;
            loop$list = rest$2;
            loop$compare = compare;
            loop$growing = prepend(new$1, List$Empty$const);
            loop$direction = direction$1;
            loop$prev = next;
            loop$acc = acc$1;
          }
        }
      } else if ($ instanceof Lt) {
        let _block;
        if (direction instanceof Ascending) {
          _block = prepend(reverse(growing$1), acc);
        } else {
          _block = prepend(growing$1, acc);
        }
        let acc$1 = _block;
        if (rest$1 instanceof Empty) {
          return prepend(prepend(new$1, List$Empty$const), acc$1);
        } else {
          let next = rest$1.head;
          let rest$2 = rest$1.tail;
          let _block$1;
          let $1 = compare(new$1, next);
          if ($1 instanceof Lt) {
            _block$1 = Sorting$Ascending$const;
          } else if ($1 instanceof Eq) {
            _block$1 = Sorting$Ascending$const;
          } else {
            _block$1 = Sorting$Descending$const;
          }
          let direction$1 = _block$1;
          loop$list = rest$2;
          loop$compare = compare;
          loop$growing = prepend(new$1, List$Empty$const);
          loop$direction = direction$1;
          loop$prev = next;
          loop$acc = acc$1;
        }
      } else if ($ instanceof Eq) {
        let _block;
        if (direction instanceof Ascending) {
          _block = prepend(reverse(growing$1), acc);
        } else {
          _block = prepend(growing$1, acc);
        }
        let acc$1 = _block;
        if (rest$1 instanceof Empty) {
          return prepend(prepend(new$1, List$Empty$const), acc$1);
        } else {
          let next = rest$1.head;
          let rest$2 = rest$1.tail;
          let _block$1;
          let $1 = compare(new$1, next);
          if ($1 instanceof Lt) {
            _block$1 = Sorting$Ascending$const;
          } else if ($1 instanceof Eq) {
            _block$1 = Sorting$Ascending$const;
          } else {
            _block$1 = Sorting$Descending$const;
          }
          let direction$1 = _block$1;
          loop$list = rest$2;
          loop$compare = compare;
          loop$growing = prepend(new$1, List$Empty$const);
          loop$direction = direction$1;
          loop$prev = next;
          loop$acc = acc$1;
        }
      } else {
        loop$list = rest$1;
        loop$compare = compare;
        loop$growing = growing$1;
        loop$direction = direction;
        loop$prev = new$1;
        loop$acc = acc;
      }
    }
  }
}
function sort(list, compare) {
  if (list instanceof Empty) {
    return list;
  } else {
    let $ = list.tail;
    if ($ instanceof Empty) {
      return list;
    } else {
      let x = list.head;
      let y = $.head;
      let rest$1 = $.tail;
      let _block;
      let $1 = compare(x, y);
      if ($1 instanceof Lt) {
        _block = Sorting$Ascending$const;
      } else if ($1 instanceof Eq) {
        _block = Sorting$Ascending$const;
      } else {
        _block = Sorting$Descending$const;
      }
      let direction = _block;
      let sequences$1 = sequences(rest$1, compare, prepend(x, List$Empty$const), direction, y, List$Empty$const);
      return merge_all(sequences$1, Sorting$Ascending$const, compare);
    }
  }
}

// build/dev/javascript/gleam_stdlib/gleam/dynamic/decode.mjs
class DecodeError extends CustomType {
  constructor(expected, found, path) {
    super();
    this.expected = expected;
    this.found = found;
    this.path = path;
  }
}
var DecodeError$DecodeError = (expected, found, path) => new DecodeError(expected, found, path);
class Decoder extends CustomType {
  constructor(function$) {
    super();
    this.function = function$;
  }
}
var float2 = /* @__PURE__ */ new Decoder(decode_float);
var int2 = /* @__PURE__ */ new Decoder(decode_int);
var string2 = /* @__PURE__ */ new Decoder(decode_string);
function run(data, decoder) {
  let $ = decoder.function(data);
  let maybe_invalid_data = $[0];
  let errors = $[1];
  if (errors instanceof Empty) {
    return new Ok(maybe_invalid_data);
  } else {
    return new Error2(errors);
  }
}
function run_dynamic_function(data, name, f) {
  let $ = f(data);
  if ($ instanceof Ok) {
    let data$1 = $[0];
    return [data$1, List$Empty$const];
  } else {
    let placeholder = $[0];
    return [
      placeholder,
      prepend(new DecodeError(name, classify_dynamic(data), List$Empty$const), List$Empty$const)
    ];
  }
}
function decode_float(data) {
  return run_dynamic_function(data, "Float", float);
}
function map3(decoder, transformer) {
  return new Decoder((d) => {
    let $ = decoder.function(d);
    let data = $[0];
    let errors = $[1];
    return [transformer(data), errors];
  });
}
function decode_int(data) {
  return run_dynamic_function(data, "Int", int);
}
function decode_string(data) {
  return run_dynamic_function(data, "String", string);
}
function run_decoders(loop$data, loop$failure, loop$decoders) {
  while (true) {
    let data = loop$data;
    let failure = loop$failure;
    let decoders = loop$decoders;
    if (decoders instanceof Empty) {
      return failure;
    } else {
      let decoder = decoders.head;
      let decoders$1 = decoders.tail;
      let $ = decoder.function(data);
      let layer = $;
      let errors = $[1];
      if (errors instanceof Empty) {
        return layer;
      } else {
        loop$data = data;
        loop$failure = failure;
        loop$decoders = decoders$1;
      }
    }
  }
}
function one_of(first, alternatives) {
  return new Decoder((dynamic_data) => {
    let $ = first.function(dynamic_data);
    let layer = $;
    let errors = $[1];
    if (errors instanceof Empty) {
      return layer;
    } else {
      return run_decoders(dynamic_data, layer, alternatives);
    }
  });
}
function path_segment_to_string(key) {
  let decoder = one_of(string2, prepend((() => {
    let _pipe = int2;
    return map3(_pipe, to_string);
  })(), prepend((() => {
    let _pipe = float2;
    return map3(_pipe, float_to_string);
  })(), List$Empty$const)));
  let $ = run(key, decoder);
  if ($ instanceof Ok) {
    let key$1 = $[0];
    return key$1;
  } else {
    return "<" + classify_dynamic(key) + ">";
  }
}
function push_path(layer, path) {
  let path$1 = map2(path, (key) => {
    let _pipe = key;
    let _pipe$1 = identity(_pipe);
    return path_segment_to_string(_pipe$1);
  });
  let errors = map2(layer[1], (error) => {
    return new DecodeError(error.expected, error.found, append(path$1, error.path));
  });
  return [layer[0], errors];
}
function list2(inner) {
  return new Decoder((data) => {
    return list(data, inner.function, (p, k) => {
      return push_path(p, prepend(k, List$Empty$const));
    }, 0, List$Empty$const);
  });
}
function index3(loop$path, loop$position, loop$inner, loop$data, loop$handle_miss) {
  while (true) {
    let path = loop$path;
    let position = loop$position;
    let inner = loop$inner;
    let data = loop$data;
    let handle_miss = loop$handle_miss;
    if (path instanceof Empty) {
      let _pipe = data;
      let _pipe$1 = inner(_pipe);
      return push_path(_pipe$1, reverse(position));
    } else {
      let key = path.head;
      let path$1 = path.tail;
      let $ = index2(data, key);
      if ($ instanceof Ok) {
        let $1 = $[0];
        if ($1 instanceof Some) {
          let data$1 = $1[0];
          loop$path = path$1;
          loop$position = prepend(key, position);
          loop$inner = inner;
          loop$data = data$1;
          loop$handle_miss = handle_miss;
        } else {
          return handle_miss(data, prepend(key, position));
        }
      } else {
        let kind = $[0];
        let $1 = inner(data);
        let default$ = $1[0];
        let _pipe = [
          default$,
          prepend(new DecodeError(kind, classify_dynamic(data), List$Empty$const), List$Empty$const)
        ];
        return push_path(_pipe, reverse(position));
      }
    }
  }
}
function subfield(field_path, field_decoder, next) {
  return new Decoder((data) => {
    let $ = index3(field_path, List$Empty$const, field_decoder.function, data, (data, position) => {
      let $1 = field_decoder.function(data);
      let default$ = $1[0];
      let _pipe = [
        default$,
        prepend(new DecodeError("Field", "Nothing", List$Empty$const), List$Empty$const)
      ];
      return push_path(_pipe, reverse(position));
    });
    let out = $[0];
    let errors1 = $[1];
    let $1 = next(out).function(data);
    let out$1 = $1[0];
    let errors2 = $1[1];
    return [out$1, append(errors1, errors2)];
  });
}
function success(data) {
  return new Decoder((_) => {
    return [data, List$Empty$const];
  });
}
function decode_error(expected, found) {
  return prepend(new DecodeError(expected, classify_dynamic(found), List$Empty$const), List$Empty$const);
}
function field(field_name, field_decoder, next) {
  return subfield(prepend(field_name, List$Empty$const), field_decoder, next);
}
function fold_dict(acc, key, value, key_decoder, value_decoder) {
  let $ = key_decoder(key);
  let $1 = $[1];
  if ($1 instanceof Empty) {
    let key_decoded = $[0];
    let $2 = value_decoder(value);
    let $3 = $2[1];
    if ($3 instanceof Empty) {
      let value$1 = $2[0];
      let dict$1 = insert(acc[0], key_decoded, value$1);
      return [dict$1, acc[1]];
    } else {
      let errors = $3;
      let key_identifier = path_segment_to_string(key);
      return push_path([make(), errors], prepend(key_identifier, List$Empty$const));
    }
  } else {
    let errors = $1;
    return push_path([make(), errors], prepend("keys", List$Empty$const));
  }
}
function dict2(key, value) {
  return new Decoder((data) => {
    let $ = dict(data);
    if ($ instanceof Ok) {
      let dict$1 = $[0];
      return fold(dict$1, [make(), List$Empty$const], (a, k, v) => {
        let $1 = a[1];
        if ($1 instanceof Empty) {
          return fold_dict(a, k, v, key.function, value.function);
        } else {
          return a;
        }
      });
    } else {
      return [make(), decode_error("Dict", data)];
    }
  });
}
function optional(inner) {
  return new Decoder((data) => {
    let $ = is_null(data);
    if ($) {
      return [Option$None$const, List$Empty$const];
    } else {
      let $1 = inner.function(data);
      let data$1 = $1[0];
      let errors = $1[1];
      return [new Some(data$1), errors];
    }
  });
}

// build/dev/javascript/gleam_stdlib/gleam_stdlib.mjs
function identity(x) {
  return x;
}
function to_string(term) {
  return term.toString();
}
function graphemes(string) {
  const iterator = graphemes_iterator(string);
  if (iterator) {
    return arrayToList(Array.from(iterator).map((item) => item.segment));
  } else {
    return arrayToList(string.match(/./gsu));
  }
}
var segmenter = undefined;
function graphemes_iterator(string) {
  if (globalThis.Intl && Intl.Segmenter) {
    segmenter ||= new Intl.Segmenter;
    return segmenter.segment(string)[Symbol.iterator]();
  }
}
function split(xs, pattern) {
  return arrayToList(xs.split(pattern));
}
function starts_with(haystack, needle) {
  return haystack.startsWith(needle);
}
var unicode_whitespaces = [
  " ",
  "\t",
  `
`,
  "\v",
  "\f",
  "\r",
  "",
  "\u2028",
  "\u2029"
].join("");
var trim_start_regex = /* @__PURE__ */ new RegExp(`^[${unicode_whitespaces}]*`);
var trim_end_regex = /* @__PURE__ */ new RegExp(`[${unicode_whitespaces}]*$`);
function classify_dynamic(data) {
  if (typeof data === "string") {
    return "String";
  } else if (typeof data === "boolean") {
    return "Bool";
  } else if (isResult(data)) {
    return "Result";
  } else if (isList(data)) {
    return "List";
  } else if (data instanceof BitArray) {
    return "BitArray";
  } else if (data instanceof Dict) {
    return "Dict";
  } else if (Number.isInteger(data)) {
    return "Int";
  } else if (Array.isArray(data)) {
    return `Array`;
  } else if (typeof data === "number") {
    return "Float";
  } else if (data === null) {
    return "Nil";
  } else if (data === undefined) {
    return "Nil";
  } else {
    const type = typeof data;
    return type.charAt(0).toUpperCase() + type.slice(1);
  }
}
var MIN_I32 = -(2 ** 31);
var MAX_I32 = 2 ** 31 - 1;
var U32 = 2 ** 32;
var MAX_SAFE = Number.MAX_SAFE_INTEGER;
var MIN_SAFE = Number.MIN_SAFE_INTEGER;
function float_to_string(float) {
  const string = float.toString().replace("+", "");
  if (string.indexOf(".") >= 0) {
    return string;
  } else {
    const index = string.indexOf("e");
    if (index >= 0) {
      return string.slice(0, index) + ".0" + string.slice(index);
    } else {
      return string + ".0";
    }
  }
}
function index2(data, key) {
  if (data instanceof Dict) {
    const result = get(data, key);
    return Result$Ok(result.isOk() ? new Some(result[0]) : new None);
  }
  if (data instanceof WeakMap || data instanceof Map) {
    const token = {};
    const entry = data.get(key, token);
    if (entry === token)
      return Result$Ok(new None);
    return Result$Ok(new Some(entry));
  }
  const key_is_int = Number.isInteger(key);
  if (key_is_int && key >= 0 && key < 8 && isList(data)) {
    let i = 0;
    for (const value of data) {
      if (i === key)
        return Result$Ok(new Some(value));
      i++;
    }
    return Result$Error("Indexable");
  }
  if (key_is_int && Array.isArray(data) || data && typeof data === "object" || data && Object.getPrototypeOf(data) === Object.prototype) {
    if (key in data)
      return Result$Ok(new Some(data[key]));
    return Result$Ok(new None);
  }
  return Result$Error(key_is_int ? "Indexable" : "Dict");
}
function list(data, decode, pushPath, index, emptyList) {
  if (!(isList(data) || Array.isArray(data))) {
    const error = DecodeError$DecodeError("List", classify_dynamic(data), emptyList);
    return [emptyList, arrayToList([error])];
  }
  const decoded = [];
  for (const element of data) {
    const layer = decode(element);
    const [out, errors] = layer;
    if (List$isNonEmpty(errors)) {
      const [_, errors] = pushPath(layer, index.toString());
      return [emptyList, errors];
    }
    decoded.push(out);
    index++;
  }
  return [arrayToList(decoded), emptyList];
}
function dict(data) {
  if (data instanceof Dict) {
    return Result$Ok(data);
  }
  if (data instanceof Map || data instanceof WeakMap) {
    return Result$Ok(from(data));
  }
  if (data == null) {
    return Result$Error("Dict");
  }
  if (typeof data !== "object") {
    return Result$Error("Dict");
  }
  const proto = Object.getPrototypeOf(data);
  if (proto === Object.prototype || proto === null) {
    return Result$Ok(from(Object.entries(data)));
  }
  return Result$Error("Dict");
}
function float(data) {
  if (typeof data === "number")
    return Result$Ok(data);
  return Result$Error(0);
}
function int(data) {
  if (Number.isInteger(data))
    return Result$Ok(data);
  return Result$Error(0);
}
function string(data) {
  if (typeof data === "string")
    return Result$Ok(data);
  return Result$Error("");
}
function is_null(data) {
  return data === null || data === undefined;
}
function arrayToList(array) {
  let list = List$Empty();
  let i = array.length;
  while (i--) {
    list = List$NonEmpty(array[i], list);
  }
  return list;
}
function isList(data) {
  return List$isEmpty(data) || List$isNonEmpty(data);
}
function isResult(data) {
  return Result$isOk(data) || Result$isError(data);
}

// build/dev/javascript/gleam_stdlib/gleam/string_tree.mjs
class All extends CustomType {
}
var Direction$All$const = new All;

// build/dev/javascript/gleam_stdlib/gleam/string.mjs
class Leading extends CustomType {
}
var Direction$Leading$const = new Leading;

class Trailing extends CustomType {
}
var Direction$Trailing$const = new Trailing;
function split2(x, substring) {
  if (substring === "") {
    return graphemes(x);
  } else {
    let _pipe = x;
    let _pipe$1 = identity(_pipe);
    let _pipe$2 = split(_pipe$1, substring);
    return map2(_pipe$2, identity);
  }
}
function concat_loop(loop$strings, loop$accumulator) {
  while (true) {
    let strings = loop$strings;
    let accumulator = loop$accumulator;
    if (strings instanceof Empty) {
      return accumulator;
    } else {
      let string = strings.head;
      let strings$1 = strings.tail;
      loop$strings = strings$1;
      loop$accumulator = accumulator + string;
    }
  }
}
function concat2(strings) {
  return concat_loop(strings, "");
}

// build/dev/javascript/gleam_stdlib/gleam/result.mjs
function map_error(result, fun) {
  if (result instanceof Ok) {
    return result;
  } else {
    let error = result[0];
    return new Error2(fun(error));
  }
}
function try$(result, fun) {
  if (result instanceof Ok) {
    let x = result[0];
    return fun(x);
  } else {
    return result;
  }
}
// build/dev/javascript/gleam_stdlib/gleam/bool.mjs
function guard(requirement, consequence, alternative) {
  if (requirement) {
    return consequence;
  } else {
    return alternative();
  }
}

// build/dev/javascript/gleam_time/gleam/time/duration.mjs
class Nanosecond extends CustomType {
}
var Unit$Nanosecond$const = new Nanosecond;
class Microsecond extends CustomType {
}
var Unit$Microsecond$const = new Microsecond;
class Millisecond extends CustomType {
}
var Unit$Millisecond$const = new Millisecond;
class Second extends CustomType {
}
var Unit$Second$const = new Second;
class Minute extends CustomType {
}
var Unit$Minute$const = new Minute;
class Hour extends CustomType {
}
var Unit$Hour$const = new Hour;
class Day extends CustomType {
}
var Unit$Day$const = new Day;
class Week extends CustomType {
}
var Unit$Week$const = new Week;
class Month extends CustomType {
}
var Unit$Month$const = new Month;
class Year extends CustomType {
}
var Unit$Year$const = new Year;

// build/dev/javascript/gleam_time/gleam/time/calendar.mjs
class Monday extends CustomType {
}
var DayOfWeek$Monday$const = new Monday;
class Tuesday extends CustomType {
}
var DayOfWeek$Tuesday$const = new Tuesday;
class Wednesday extends CustomType {
}
var DayOfWeek$Wednesday$const = new Wednesday;
class Thursday extends CustomType {
}
var DayOfWeek$Thursday$const = new Thursday;
class Friday extends CustomType {
}
var DayOfWeek$Friday$const = new Friday;
class Saturday extends CustomType {
}
var DayOfWeek$Saturday$const = new Saturday;
class Sunday extends CustomType {
}
var DayOfWeek$Sunday$const = new Sunday;
class January extends CustomType {
}
var Month$January$const = new January;
class February extends CustomType {
}
var Month$February$const = new February;
class March extends CustomType {
}
var Month$March$const = new March;
class April extends CustomType {
}
var Month$April$const = new April;
class May extends CustomType {
}
var Month$May$const = new May;
class June extends CustomType {
}
var Month$June$const = new June;
class July extends CustomType {
}
var Month$July$const = new July;
class August extends CustomType {
}
var Month$August$const = new August;
class September extends CustomType {
}
var Month$September$const = new September;
class October extends CustomType {
}
var Month$October$const = new October;
class November extends CustomType {
}
var Month$November$const = new November;
class December extends CustomType {
}
var Month$December$const = new December;

// build/dev/javascript/gleam_time/gleam/time/timestamp.mjs
class Timestamp extends CustomType {
  constructor(seconds, nanoseconds) {
    super();
    this.seconds = seconds;
    this.nanoseconds = nanoseconds;
  }
}
function from_unix_seconds(seconds) {
  return new Timestamp(seconds, 0);
}
// build/dev/javascript/gleam_json/gleam_json_ffi.mjs
function decode(string) {
  try {
    const result = JSON.parse(string);
    return Result$Ok(result);
  } catch (err) {
    return Result$Error(getJsonDecodeError(err, string));
  }
}
function getJsonDecodeError(stdErr, json) {
  if (isUnexpectedEndOfInput(stdErr))
    return DecodeError$UnexpectedEndOfInput();
  return toUnexpectedByteError(stdErr, json);
}
function isUnexpectedEndOfInput(err) {
  const unexpectedEndOfInputRegex = /((unexpected (end|eof))|(end of data)|(unterminated string)|(json( parse error|\.parse)\: expected '(\:|\}|\])'))/i;
  return unexpectedEndOfInputRegex.test(err.message);
}
function toUnexpectedByteError(err, json) {
  let converters = [
    v8UnexpectedByteError,
    oldV8UnexpectedByteError,
    jsCoreUnexpectedByteError,
    spidermonkeyUnexpectedByteError
  ];
  for (let converter of converters) {
    let result = converter(err, json);
    if (result)
      return result;
  }
  return DecodeError$UnexpectedByte("");
}
function v8UnexpectedByteError(err) {
  const regex = /unexpected token '(.)', ".+" is not valid JSON/i;
  const match = regex.exec(err.message);
  if (!match)
    return null;
  const byte = toHex(match[1]);
  return DecodeError$UnexpectedByte(byte);
}
function oldV8UnexpectedByteError(err) {
  const regex = /unexpected token (.) in JSON at position (\d+)/i;
  const match = regex.exec(err.message);
  if (!match)
    return null;
  const byte = toHex(match[1]);
  return DecodeError$UnexpectedByte(byte);
}
function spidermonkeyUnexpectedByteError(err, json) {
  const regex = /(unexpected character|expected .*) at line (\d+) column (\d+)/i;
  const match = regex.exec(err.message);
  if (!match)
    return null;
  const line = Number(match[2]);
  const column = Number(match[3]);
  const position = getPositionFromMultiline(line, column, json);
  const byte = toHex(json[position]);
  return DecodeError$UnexpectedByte(byte);
}
function jsCoreUnexpectedByteError(err) {
  const regex = /unexpected (identifier|token) "(.)"/i;
  const match = regex.exec(err.message);
  if (!match)
    return null;
  const byte = toHex(match[2]);
  return DecodeError$UnexpectedByte(byte);
}
function toHex(char) {
  return "0x" + char.charCodeAt(0).toString(16).toUpperCase();
}
function getPositionFromMultiline(line, column, string) {
  if (line === 1)
    return column - 1;
  let currentLn = 1;
  let position = 0;
  string.split("").find((char, idx) => {
    if (char === `
`)
      currentLn += 1;
    if (currentLn === line) {
      position = idx + column;
      return true;
    }
    return false;
  });
  return position;
}

// build/dev/javascript/gleam_json/gleam/json.mjs
class UnexpectedEndOfInput extends CustomType {
}
var DecodeError$UnexpectedEndOfInput$const = new UnexpectedEndOfInput;
var DecodeError$UnexpectedEndOfInput = () => DecodeError$UnexpectedEndOfInput$const;
class UnexpectedByte extends CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
var DecodeError$UnexpectedByte = ($0) => new UnexpectedByte($0);
class UnableToDecode extends CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
function do_parse(json, decoder) {
  return try$(decode(json), (dynamic_value) => {
    let _pipe = run(dynamic_value, decoder);
    return map_error(_pipe, (var0) => {
      return new UnableToDecode(var0);
    });
  });
}
function parse(json, decoder) {
  return do_parse(json, decoder);
}
// build/dev/javascript/houdini/houdini.ffi.mjs
function escape(string) {
  return string.replaceAll(/[><&"']/g, (replaced) => {
    switch (replaced) {
      case ">":
        return "&gt;";
      case "<":
        return "&lt;";
      case "'":
        return "&#39;";
      case "&":
        return "&amp;";
      case '"':
        return "&quot;";
      default:
        return replaced;
    }
  });
}

// build/dev/javascript/lustre/lustre/internals/constants.mjs
var empty_list = List$Empty$const;
var error_nil = /* @__PURE__ */ new Error2(undefined);
function singleton_list(item) {
  return prepend(item, empty_list);
}

// build/dev/javascript/lustre/lustre/vdom/vattr.ffi.mjs
var GT = /* @__PURE__ */ Order$Gt();
var LT = /* @__PURE__ */ Order$Lt();
var EQ = /* @__PURE__ */ Order$Eq();
function compare3(a, b) {
  if (a.name === b.name) {
    return EQ;
  } else if (a.name < b.name) {
    return LT;
  } else {
    return GT;
  }
}

// build/dev/javascript/lustre/lustre/vdom/vattr.mjs
class Attribute extends CustomType {
  constructor(kind, name, value) {
    super();
    this.kind = kind;
    this.name = name;
    this.value = value;
  }
}
class Property extends CustomType {
  constructor(kind, name, value) {
    super();
    this.kind = kind;
    this.name = name;
    this.value = value;
  }
}
class Event2 extends CustomType {
  constructor(kind, name, handler, include, prevent_default, stop_propagation, debounce, throttle) {
    super();
    this.kind = kind;
    this.name = name;
    this.handler = handler;
    this.include = include;
    this.prevent_default = prevent_default;
    this.stop_propagation = stop_propagation;
    this.debounce = debounce;
    this.throttle = throttle;
  }
}
class Handler extends CustomType {
  constructor(prevent_default, stop_propagation, message) {
    super();
    this.prevent_default = prevent_default;
    this.stop_propagation = stop_propagation;
    this.message = message;
  }
}
class Never extends CustomType {
  constructor(kind) {
    super();
    this.kind = kind;
  }
}
var attribute_kind = 0;
var property_kind = 1;
var event_kind = 2;
var never_kind = 0;
var never = /* @__PURE__ */ new Never(never_kind);
var always_kind = 2;
function attribute(name, value) {
  return new Attribute(attribute_kind, name, value);
}
function event(name, handler, include, prevent_default, stop_propagation, debounce, throttle) {
  return new Event2(event_kind, name, handler, include, prevent_default, stop_propagation, debounce, throttle);
}
function merge(loop$attributes, loop$merged) {
  while (true) {
    let attributes = loop$attributes;
    let merged = loop$merged;
    if (attributes instanceof Empty) {
      return merged;
    } else {
      let $ = attributes.head;
      if ($ instanceof Attribute) {
        let $1 = $.name;
        if ($1 === "") {
          let rest = attributes.tail;
          loop$attributes = rest;
          loop$merged = merged;
        } else if ($1 === "class") {
          let $2 = $.value;
          if ($2 === "") {
            let rest = attributes.tail;
            loop$attributes = rest;
            loop$merged = merged;
          } else {
            let $3 = attributes.tail;
            if ($3 instanceof Empty) {
              let attribute$1 = $;
              let rest = $3;
              loop$attributes = rest;
              loop$merged = prepend(attribute$1, merged);
            } else {
              let $4 = $3.head;
              if ($4 instanceof Attribute && $4.name === "class") {
                let kind = $.kind;
                let class1 = $2;
                let rest = $3.tail;
                let class2 = $4.value;
                let value = class1 + " " + class2;
                let attribute$1 = new Attribute(kind, "class", value);
                loop$attributes = prepend(attribute$1, rest);
                loop$merged = merged;
              } else {
                let attribute$1 = $;
                let rest = $3;
                loop$attributes = rest;
                loop$merged = prepend(attribute$1, merged);
              }
            }
          }
        } else if ($1 === "style") {
          let $2 = $.value;
          if ($2 === "") {
            let rest = attributes.tail;
            loop$attributes = rest;
            loop$merged = merged;
          } else {
            let $3 = attributes.tail;
            if ($3 instanceof Empty) {
              let attribute$1 = $;
              let rest = $3;
              loop$attributes = rest;
              loop$merged = prepend(attribute$1, merged);
            } else {
              let $4 = $3.head;
              if ($4 instanceof Attribute && $4.name === "style") {
                let kind = $.kind;
                let style1 = $2;
                let rest = $3.tail;
                let style2 = $4.value;
                let value = style1 + ";" + style2;
                let attribute$1 = new Attribute(kind, "style", value);
                loop$attributes = prepend(attribute$1, rest);
                loop$merged = merged;
              } else {
                let attribute$1 = $;
                let rest = $3;
                loop$attributes = rest;
                loop$merged = prepend(attribute$1, merged);
              }
            }
          }
        } else {
          let attribute$1 = $;
          let rest = attributes.tail;
          loop$attributes = rest;
          loop$merged = prepend(attribute$1, merged);
        }
      } else {
        let attribute$1 = $;
        let rest = attributes.tail;
        loop$attributes = rest;
        loop$merged = prepend(attribute$1, merged);
      }
    }
  }
}
function prepare(attributes) {
  if (attributes instanceof Empty) {
    return attributes;
  } else if (attributes.tail instanceof Empty) {
    return attributes;
  } else {
    let _pipe = attributes;
    let _pipe$1 = sort(_pipe, (a, b) => {
      return compare3(b, a);
    });
    return merge(_pipe$1, empty_list);
  }
}

// build/dev/javascript/lustre/lustre/attribute.mjs
function attribute2(name, value) {
  return attribute(name, value);
}
function class$(name) {
  return attribute2("class", name);
}
function id(value) {
  return attribute2("id", value);
}
function href(url) {
  return attribute2("href", url);
}

// build/dev/javascript/lustre/lustre/internals/mutable_map.ffi.mjs
function empty2() {
  return null;
}
function get2(map, key) {
  return map?.get(key);
}
function get_or_compute(map, key, compute) {
  return map?.get(key) ?? compute();
}
function has_key(map, key) {
  return map && map.has(key);
}
function insert2(map, key, value) {
  map ??= new Map;
  map.set(key, value);
  return map;
}
function remove(map, key) {
  map?.delete(key);
  return map;
}

// build/dev/javascript/gleam_stdlib/gleam/function.mjs
function identity3(x) {
  return x;
}

// build/dev/javascript/lustre/lustre/internals/ref.ffi.mjs
function sameValueZero(x, y) {
  if (typeof x === "number" && typeof y === "number") {
    return x === y || x !== x && y !== y;
  }
  return x === y;
}

// build/dev/javascript/lustre/lustre/internals/ref.mjs
function equal_lists(loop$xs, loop$ys) {
  while (true) {
    let xs = loop$xs;
    let ys = loop$ys;
    if (xs instanceof Empty) {
      if (ys instanceof Empty) {
        return true;
      } else {
        return false;
      }
    } else if (ys instanceof Empty) {
      return false;
    } else {
      let x = xs.head;
      let xs$1 = xs.tail;
      let y = ys.head;
      let ys$1 = ys.tail;
      let $ = sameValueZero(x, y);
      if ($) {
        loop$xs = xs$1;
        loop$ys = ys$1;
      } else {
        return $;
      }
    }
  }
}

// build/dev/javascript/lustre/lustre/vdom/vnode.mjs
class Fragment extends CustomType {
  constructor(kind, key, children, keyed_children) {
    super();
    this.kind = kind;
    this.key = key;
    this.children = children;
    this.keyed_children = keyed_children;
  }
}
class Element2 extends CustomType {
  constructor(kind, key, namespace, tag, attributes, children, keyed_children, self_closing, void$) {
    super();
    this.kind = kind;
    this.key = key;
    this.namespace = namespace;
    this.tag = tag;
    this.attributes = attributes;
    this.children = children;
    this.keyed_children = keyed_children;
    this.self_closing = self_closing;
    this.void = void$;
  }
}
class Text extends CustomType {
  constructor(kind, key, content) {
    super();
    this.kind = kind;
    this.key = key;
    this.content = content;
  }
}
class UnsafeInnerHtml extends CustomType {
  constructor(kind, key, namespace, tag, attributes, inner_html) {
    super();
    this.kind = kind;
    this.key = key;
    this.namespace = namespace;
    this.tag = tag;
    this.attributes = attributes;
    this.inner_html = inner_html;
  }
}
class Map2 extends CustomType {
  constructor(kind, key, mapper, child) {
    super();
    this.kind = kind;
    this.key = key;
    this.mapper = mapper;
    this.child = child;
  }
}
class Memo extends CustomType {
  constructor(kind, key, dependencies, view) {
    super();
    this.kind = kind;
    this.key = key;
    this.dependencies = dependencies;
    this.view = view;
  }
}
var fragment_kind = 0;
var element_kind = 1;
var text_kind = 2;
var unsafe_inner_html_kind = 3;
var map_kind = 4;
var memo_kind = 5;
function fragment(key, children, keyed_children) {
  return new Fragment(fragment_kind, key, children, keyed_children);
}
function element(key, namespace, tag, attributes, children, keyed_children, self_closing, void$) {
  return new Element2(element_kind, key, namespace, tag, prepare(attributes), children, keyed_children, self_closing, void$);
}
function is_void_html_element(tag, namespace) {
  if (namespace === "") {
    if (tag === "area") {
      return true;
    } else if (tag === "base") {
      return true;
    } else if (tag === "br") {
      return true;
    } else if (tag === "col") {
      return true;
    } else if (tag === "embed") {
      return true;
    } else if (tag === "hr") {
      return true;
    } else if (tag === "img") {
      return true;
    } else if (tag === "input") {
      return true;
    } else if (tag === "link") {
      return true;
    } else if (tag === "meta") {
      return true;
    } else if (tag === "param") {
      return true;
    } else if (tag === "source") {
      return true;
    } else if (tag === "track") {
      return true;
    } else if (tag === "wbr") {
      return true;
    } else {
      return false;
    }
  } else {
    return false;
  }
}
function text(key, content) {
  return new Text(text_kind, key, content);
}
function map4(element, mapper) {
  if (element instanceof Map2) {
    let child_mapper = element.mapper;
    return new Map2(map_kind, element.key, (handler) => {
      return identity3(mapper)(child_mapper(handler));
    }, identity3(element.child));
  } else {
    return new Map2(map_kind, element.key, identity3(mapper), identity3(element));
  }
}
function memo(key, dependencies, view) {
  return new Memo(memo_kind, key, dependencies, view);
}
function to_keyed(key, node) {
  if (node instanceof Fragment) {
    return new Fragment(node.kind, key, node.children, node.keyed_children);
  } else if (node instanceof Element2) {
    return new Element2(node.kind, key, node.namespace, node.tag, node.attributes, node.children, node.keyed_children, node.self_closing, node.void);
  } else if (node instanceof Text) {
    return new Text(node.kind, key, node.content);
  } else if (node instanceof UnsafeInnerHtml) {
    return new UnsafeInnerHtml(node.kind, key, node.namespace, node.tag, node.attributes, node.inner_html);
  } else if (node instanceof Map2) {
    let child = node.child;
    return new Map2(node.kind, key, node.mapper, to_keyed(key, child));
  } else {
    let view = node.view;
    return new Memo(node.kind, key, node.dependencies, () => {
      return to_keyed(key, view());
    });
  }
}

// build/dev/javascript/lustre/lustre/element.mjs
class Html extends CustomType {
}
var DocumentType$Html$const = new Html;

class HeadOnly extends CustomType {
}
var DocumentType$HeadOnly$const = new HeadOnly;

class BodyOnly extends CustomType {
}
var DocumentType$BodyOnly$const = new BodyOnly;

class HeadAndBody extends CustomType {
}
var DocumentType$HeadAndBody$const = new HeadAndBody;

class Other extends CustomType {
}
var DocumentType$Other$const = new Other;
function element2(tag, attributes, children) {
  return element("", "", tag, attributes, children, empty2(), false, is_void_html_element(tag, ""));
}
function text2(content) {
  return text("", content);
}
function none() {
  return text("", "");
}
function memo2(dependencies, view) {
  return memo("", dependencies, view);
}
function ref(value) {
  return identity3(value);
}
function map5(element, f) {
  return map4(element, f);
}
// build/dev/javascript/blogatto/blogatto/post.mjs
class Post extends CustomType {
  constructor(title, slug, url, date, description, excerpt, language, featured_image, contents, extras) {
    super();
    this.title = title;
    this.slug = slug;
    this.url = url;
    this.date = date;
    this.description = description;
    this.excerpt = excerpt;
    this.language = language;
    this.featured_image = featured_image;
    this.contents = contents;
    this.extras = extras;
  }
}
// build/dev/javascript/gleam_erlang/gleam/erlang/process.mjs
class Normal extends CustomType {
}
var ExitReason$Normal$const = new Normal;
class Killed extends CustomType {
}
var ExitReason$Killed$const = new Killed;
class Anything extends CustomType {
}
var AnythingSelectorTag$Anything$const = new Anything;

class Process extends CustomType {
}
var ProcessMonitorFlag$Process$const = new Process;
class TimerNotFound extends CustomType {
}
var Cancelled$TimerNotFound$const = new TimerNotFound;
class Kill extends CustomType {
}
var KillFlag$Kill$const = new Kill;
// build/dev/javascript/gleam_otp/gleam/otp/system.mjs
class Running extends CustomType {
}
var Mode$Running$const = new Running;
class Suspended extends CustomType {
}
var Mode$Suspended$const = new Suspended;
class NoDebug extends CustomType {
}
var DebugOption$NoDebug$const = new NoDebug;

// build/dev/javascript/gleam_otp/gleam/otp/actor.mjs
class InitTimeout extends CustomType {
}
var StartError$InitTimeout$const = new InitTimeout;

// build/dev/javascript/gleam_otp/gleam/otp/supervision.mjs
class Permanent extends CustomType {
}
var Restart$Permanent$const = new Permanent;
class Transient extends CustomType {
}
var Restart$Transient$const = new Transient;
class Temporary extends CustomType {
}
var Restart$Temporary$const = new Temporary;
class Supervisor extends CustomType {
}
var ChildType$Supervisor$const = new Supervisor;

// build/dev/javascript/gleam_otp/gleam/otp/factory_supervisor.mjs
class SimpleOneForOne extends CustomType {
}
var Strategy$SimpleOneForOne$const = new SimpleOneForOne;

// build/dev/javascript/lustre/lustre/effect.mjs
class Effect extends CustomType {
  constructor(synchronous, before_paint, after_paint) {
    super();
    this.synchronous = synchronous;
    this.before_paint = before_paint;
    this.after_paint = after_paint;
  }
}
var empty3 = /* @__PURE__ */ new Effect(empty_list, empty_list, empty_list);
function none2() {
  return empty3;
}
function after_paint(effect) {
  let task = (actions) => {
    let root = actions.root();
    let dispatch = actions.dispatch;
    return effect(dispatch, root);
  };
  return new Effect(empty3.synchronous, empty3.before_paint, singleton_list(task));
}

// build/dev/javascript/lustre/lustre/element/html.mjs
function h2(attrs, children) {
  return element2("h2", attrs, children);
}
function div(attrs, children) {
  return element2("div", attrs, children);
}
function p(attrs, children) {
  return element2("p", attrs, children);
}
function a(attrs, children) {
  return element2("a", attrs, children);
}
function button(attrs, children) {
  return element2("button", attrs, children);
}

// build/dev/javascript/lustre/lustre/vdom/patch.mjs
class Patch extends CustomType {
  constructor(index, path, removed, changes, children) {
    super();
    this.index = index;
    this.path = path;
    this.removed = removed;
    this.changes = changes;
    this.children = children;
  }
}
class ReplaceText extends CustomType {
  constructor(kind, content) {
    super();
    this.kind = kind;
    this.content = content;
  }
}
class ReplaceInnerHtml extends CustomType {
  constructor(kind, inner_html) {
    super();
    this.kind = kind;
    this.inner_html = inner_html;
  }
}
class Update extends CustomType {
  constructor(kind, added, removed) {
    super();
    this.kind = kind;
    this.added = added;
    this.removed = removed;
  }
}
class Move extends CustomType {
  constructor(kind, key, before) {
    super();
    this.kind = kind;
    this.key = key;
    this.before = before;
  }
}
class Replace extends CustomType {
  constructor(kind, index, with$) {
    super();
    this.kind = kind;
    this.index = index;
    this.with = with$;
  }
}
class Remove extends CustomType {
  constructor(kind, index) {
    super();
    this.kind = kind;
    this.index = index;
  }
}
class Insert extends CustomType {
  constructor(kind, children, before) {
    super();
    this.kind = kind;
    this.children = children;
    this.before = before;
  }
}
var replace_text_kind = 0;
var replace_inner_html_kind = 1;
var update_kind = 2;
var move_kind = 3;
var remove_kind = 4;
var replace_kind = 5;
var insert_kind = 6;
function new$3(index, removed, changes, children) {
  return new Patch(index, empty_list, removed, changes, children);
}
function replace_text(content) {
  return new ReplaceText(replace_text_kind, content);
}
function replace_inner_html(inner_html) {
  return new ReplaceInnerHtml(replace_inner_html_kind, inner_html);
}
function update(added, removed) {
  return new Update(update_kind, added, removed);
}
function move(key, before) {
  return new Move(move_kind, key, before);
}
function remove2(index) {
  return new Remove(remove_kind, index);
}
function replace2(index, with$) {
  return new Replace(replace_kind, index, with$);
}
function insert3(children, before) {
  return new Insert(insert_kind, children, before);
}
function add_parent(child, index) {
  return new Patch(index, prepend(child.index, child.path), child.removed, child.changes, child.children);
}

// build/dev/javascript/lustre/lustre/vdom/path.mjs
class Root extends CustomType {
}
var Path$Root$const = new Root;

class Key extends CustomType {
  constructor(key, parent) {
    super();
    this.key = key;
    this.parent = parent;
  }
}

class Index extends CustomType {
  constructor(index, parent) {
    super();
    this.index = index;
    this.parent = parent;
  }
}

class Subtree extends CustomType {
  constructor(parent) {
    super();
    this.parent = parent;
  }
}
var separator_subtree = "\r";
var separator_element = "\t";
var separator_event = `
`;
var root = Path$Root$const;
function finish_to_string(acc) {
  if (acc instanceof Empty) {
    return "";
  } else {
    let segments = acc.tail;
    return concat2(segments);
  }
}
function do_to_string(loop$full, loop$path, loop$acc) {
  while (true) {
    let full = loop$full;
    let path = loop$path;
    let acc = loop$acc;
    if (path instanceof Root) {
      return finish_to_string(acc);
    } else if (path instanceof Key) {
      let key = path.key;
      let parent = path.parent;
      loop$full = full;
      loop$path = parent;
      loop$acc = prepend(separator_element, prepend(key, acc));
    } else if (path instanceof Index) {
      let index = path.index;
      let parent = path.parent;
      let acc$1 = prepend(separator_element, prepend(to_string(index), acc));
      loop$full = full;
      loop$path = parent;
      loop$acc = acc$1;
    } else if (!full) {
      return finish_to_string(acc);
    } else {
      let parent = path.parent;
      if (acc instanceof Empty) {
        loop$full = full;
        loop$path = parent;
        loop$acc = acc;
      } else {
        let acc$1 = acc.tail;
        loop$full = full;
        loop$path = parent;
        loop$acc = prepend(separator_subtree, acc$1);
      }
    }
  }
}
function to_string3(path) {
  return do_to_string(true, path, empty_list);
}
function do_matches(loop$path, loop$candidates) {
  while (true) {
    let path = loop$path;
    let candidates = loop$candidates;
    if (candidates instanceof Empty) {
      return false;
    } else {
      let candidate = candidates.head;
      let rest = candidates.tail;
      let $ = starts_with(path, candidate);
      if ($) {
        return $;
      } else {
        loop$path = path;
        loop$candidates = rest;
      }
    }
  }
}
function matches(path, candidates) {
  if (candidates instanceof Empty) {
    return false;
  } else {
    return do_matches(to_string3(path), candidates);
  }
}
function split_subtree_path(path) {
  return split2(path, separator_subtree);
}
function add3(parent, index, key) {
  if (key === "") {
    return new Index(index, parent);
  } else {
    return new Key(key, parent);
  }
}
function subtree(path) {
  return new Subtree(path);
}
function event2(path, event) {
  return do_to_string(false, path, prepend(separator_event, prepend(event, empty_list)));
}
function child(path) {
  return do_to_string(false, path, empty_list);
}

// build/dev/javascript/lustre/lustre/vdom/cache.mjs
class Cache extends CustomType {
  constructor(events, vdoms, old_vdoms, dispatched_paths, next_dispatched_paths) {
    super();
    this.events = events;
    this.vdoms = vdoms;
    this.old_vdoms = old_vdoms;
    this.dispatched_paths = dispatched_paths;
    this.next_dispatched_paths = next_dispatched_paths;
  }
}

class Events extends CustomType {
  constructor(handlers, children) {
    super();
    this.handlers = handlers;
    this.children = children;
  }
}

class Child extends CustomType {
  constructor(mapper, events) {
    super();
    this.mapper = mapper;
    this.events = events;
  }
}

class AddedChildren extends CustomType {
  constructor(handlers, children, vdoms) {
    super();
    this.handlers = handlers;
    this.children = children;
    this.vdoms = vdoms;
  }
}

class DecodedEvent extends CustomType {
  constructor(path, handler) {
    super();
    this.path = path;
    this.handler = handler;
  }
}

class DispatchedEvent extends CustomType {
  constructor(path) {
    super();
    this.path = path;
  }
}
function compose_mapper(mapper, child_mapper) {
  return (message) => {
    return mapper(child_mapper(message));
  };
}
function new_events() {
  return new Events(empty2(), empty2());
}
function new$4() {
  return new Cache(new_events(), empty2(), empty2(), empty_list, empty_list);
}
function do_add_event(handlers, path, name, handler) {
  return insert2(handlers, event2(path, name), handler);
}
function add_attributes(handlers, path, attributes) {
  return fold2(attributes, handlers, (events, attribute) => {
    if (attribute instanceof Event2) {
      let name = attribute.name;
      let handler = attribute.handler;
      return do_add_event(events, path, name, handler);
    } else {
      return events;
    }
  });
}
function do_add_children(loop$handlers, loop$children, loop$vdoms, loop$parent, loop$child_index, loop$nodes) {
  while (true) {
    let handlers = loop$handlers;
    let children = loop$children;
    let vdoms = loop$vdoms;
    let parent = loop$parent;
    let child_index = loop$child_index;
    let nodes = loop$nodes;
    let next = child_index + 1;
    if (nodes instanceof Empty) {
      return new AddedChildren(handlers, children, vdoms);
    } else {
      let $ = nodes.head;
      if ($ instanceof Fragment) {
        let rest = nodes.tail;
        let key = $.key;
        let nodes$1 = $.children;
        let path = add3(parent, child_index, key);
        let $1 = do_add_children(handlers, children, vdoms, path, 0, nodes$1);
        let handlers$1 = $1.handlers;
        let children$1 = $1.children;
        let vdoms$1 = $1.vdoms;
        loop$handlers = handlers$1;
        loop$children = children$1;
        loop$vdoms = vdoms$1;
        loop$parent = parent;
        loop$child_index = next;
        loop$nodes = rest;
      } else if ($ instanceof Element2) {
        let rest = nodes.tail;
        let key = $.key;
        let attributes = $.attributes;
        let nodes$1 = $.children;
        let path = add3(parent, child_index, key);
        let handlers$1 = add_attributes(handlers, path, attributes);
        let $1 = do_add_children(handlers$1, children, vdoms, path, 0, nodes$1);
        let handlers$2 = $1.handlers;
        let children$1 = $1.children;
        let vdoms$1 = $1.vdoms;
        loop$handlers = handlers$2;
        loop$children = children$1;
        loop$vdoms = vdoms$1;
        loop$parent = parent;
        loop$child_index = next;
        loop$nodes = rest;
      } else if ($ instanceof Text) {
        let rest = nodes.tail;
        loop$handlers = handlers;
        loop$children = children;
        loop$vdoms = vdoms;
        loop$parent = parent;
        loop$child_index = next;
        loop$nodes = rest;
      } else if ($ instanceof UnsafeInnerHtml) {
        let rest = nodes.tail;
        let key = $.key;
        let attributes = $.attributes;
        let path = add3(parent, child_index, key);
        let handlers$1 = add_attributes(handlers, path, attributes);
        loop$handlers = handlers$1;
        loop$children = children;
        loop$vdoms = vdoms;
        loop$parent = parent;
        loop$child_index = next;
        loop$nodes = rest;
      } else if ($ instanceof Map2) {
        let rest = nodes.tail;
        let key = $.key;
        let mapper = $.mapper;
        let child2 = $.child;
        let path = add3(parent, child_index, key);
        let added = do_add_children(empty2(), empty2(), vdoms, subtree(path), 0, singleton_list(child2));
        let vdoms$1 = added.vdoms;
        let child_events = new Events(added.handlers, added.children);
        let child$1 = new Child(mapper, child_events);
        let children$1 = insert2(children, child(path), child$1);
        loop$handlers = handlers;
        loop$children = children$1;
        loop$vdoms = vdoms$1;
        loop$parent = parent;
        loop$child_index = next;
        loop$nodes = rest;
      } else {
        let rest = nodes.tail;
        let view = $.view;
        let child_node = view();
        let vdoms$1 = insert2(vdoms, view, child_node);
        let next$1 = child_index;
        let rest$1 = prepend(child_node, rest);
        loop$handlers = handlers;
        loop$children = children;
        loop$vdoms = vdoms$1;
        loop$parent = parent;
        loop$child_index = next$1;
        loop$nodes = rest$1;
      }
    }
  }
}
function add_children(cache, events, path, child_index, nodes) {
  let vdoms = cache.vdoms;
  let handlers = events.handlers;
  let children = events.children;
  let $ = do_add_children(handlers, children, vdoms, path, child_index, nodes);
  let handlers$1 = $.handlers;
  let children$1 = $.children;
  let vdoms$1 = $.vdoms;
  return [
    new Cache(cache.events, vdoms$1, cache.old_vdoms, cache.dispatched_paths, cache.next_dispatched_paths),
    new Events(handlers$1, children$1)
  ];
}
function add_child(cache, events, parent, index, child) {
  let children = singleton_list(child);
  return add_children(cache, events, parent, index, children);
}
function tick(cache) {
  return new Cache(cache.events, empty2(), cache.vdoms, cache.next_dispatched_paths, empty_list);
}
function events(cache) {
  return cache.events;
}
function update_events(cache, events) {
  return new Cache(events, cache.vdoms, cache.old_vdoms, cache.dispatched_paths, cache.next_dispatched_paths);
}
function memos(cache) {
  return cache.vdoms;
}
function get_old_memo(cache, old, new$) {
  return get_or_compute(cache.old_vdoms, old, new$);
}
function keep_memo(cache, old, new$) {
  let node = get_or_compute(cache.old_vdoms, old, new$);
  let vdoms = insert2(cache.vdoms, new$, node);
  return new Cache(cache.events, vdoms, cache.old_vdoms, cache.dispatched_paths, cache.next_dispatched_paths);
}
function add_memo(cache, new$, node) {
  let vdoms = insert2(cache.vdoms, new$, node);
  return new Cache(cache.events, vdoms, cache.old_vdoms, cache.dispatched_paths, cache.next_dispatched_paths);
}
function get_subtree(events, path, old_mapper) {
  let child = get_or_compute(events.children, path, () => {
    return new Child(old_mapper, new_events());
  });
  return child.events;
}
function update_subtree(parent, path, mapper, events) {
  let new_child = new Child(mapper, events);
  let children = insert2(parent.children, path, new_child);
  return new Events(parent.handlers, children);
}
function add_event(events, path, name, handler) {
  let handlers = do_add_event(events.handlers, path, name, handler);
  return new Events(handlers, events.children);
}
function do_remove_event(handlers, path, name) {
  return remove(handlers, event2(path, name));
}
function remove_event(events, path, name) {
  let handlers = do_remove_event(events.handlers, path, name);
  return new Events(handlers, events.children);
}
function remove_attributes(handlers, path, attributes) {
  return fold2(attributes, handlers, (events, attribute) => {
    if (attribute instanceof Event2) {
      let name = attribute.name;
      return do_remove_event(events, path, name);
    } else {
      return events;
    }
  });
}
function do_remove_children(loop$handlers, loop$children, loop$vdoms, loop$parent, loop$index, loop$nodes) {
  while (true) {
    let handlers = loop$handlers;
    let children = loop$children;
    let vdoms = loop$vdoms;
    let parent = loop$parent;
    let index = loop$index;
    let nodes = loop$nodes;
    let next = index + 1;
    if (nodes instanceof Empty) {
      return new Events(handlers, children);
    } else {
      let $ = nodes.head;
      if ($ instanceof Fragment) {
        let rest = nodes.tail;
        let key = $.key;
        let nodes$1 = $.children;
        let path = add3(parent, index, key);
        let $1 = do_remove_children(handlers, children, vdoms, path, 0, nodes$1);
        let handlers$1 = $1.handlers;
        let children$1 = $1.children;
        loop$handlers = handlers$1;
        loop$children = children$1;
        loop$vdoms = vdoms;
        loop$parent = parent;
        loop$index = next;
        loop$nodes = rest;
      } else if ($ instanceof Element2) {
        let rest = nodes.tail;
        let key = $.key;
        let attributes = $.attributes;
        let nodes$1 = $.children;
        let path = add3(parent, index, key);
        let handlers$1 = remove_attributes(handlers, path, attributes);
        let $1 = do_remove_children(handlers$1, children, vdoms, path, 0, nodes$1);
        let handlers$2 = $1.handlers;
        let children$1 = $1.children;
        loop$handlers = handlers$2;
        loop$children = children$1;
        loop$vdoms = vdoms;
        loop$parent = parent;
        loop$index = next;
        loop$nodes = rest;
      } else if ($ instanceof Text) {
        let rest = nodes.tail;
        loop$handlers = handlers;
        loop$children = children;
        loop$vdoms = vdoms;
        loop$parent = parent;
        loop$index = next;
        loop$nodes = rest;
      } else if ($ instanceof UnsafeInnerHtml) {
        let rest = nodes.tail;
        let key = $.key;
        let attributes = $.attributes;
        let path = add3(parent, index, key);
        let handlers$1 = remove_attributes(handlers, path, attributes);
        loop$handlers = handlers$1;
        loop$children = children;
        loop$vdoms = vdoms;
        loop$parent = parent;
        loop$index = next;
        loop$nodes = rest;
      } else if ($ instanceof Map2) {
        let rest = nodes.tail;
        let key = $.key;
        let path = add3(parent, index, key);
        let children$1 = remove(children, child(path));
        loop$handlers = handlers;
        loop$children = children$1;
        loop$vdoms = vdoms;
        loop$parent = parent;
        loop$index = next;
        loop$nodes = rest;
      } else {
        let rest = nodes.tail;
        let view = $.view;
        let $1 = has_key(vdoms, view);
        if ($1) {
          let child = get2(vdoms, view);
          let nodes$1 = prepend(child, rest);
          loop$handlers = handlers;
          loop$children = children;
          loop$vdoms = vdoms;
          loop$parent = parent;
          loop$index = index;
          loop$nodes = nodes$1;
        } else {
          loop$handlers = handlers;
          loop$children = children;
          loop$vdoms = vdoms;
          loop$parent = parent;
          loop$index = next;
          loop$nodes = rest;
        }
      }
    }
  }
}
function remove_child(cache, events, parent, child_index, child) {
  return do_remove_children(events.handlers, events.children, cache.old_vdoms, parent, child_index, singleton_list(child));
}
function replace_child(cache, events, parent, child_index, prev, next) {
  let events$1 = remove_child(cache, events, parent, child_index, prev);
  return add_child(cache, events$1, parent, child_index, next);
}
function get_handler(loop$events, loop$path, loop$mapper) {
  while (true) {
    let events = loop$events;
    let path = loop$path;
    let mapper = loop$mapper;
    if (path instanceof Empty) {
      return error_nil;
    } else {
      let $ = path.tail;
      if ($ instanceof Empty) {
        let key = path.head;
        let $1 = has_key(events.handlers, key);
        if ($1) {
          let handler = get2(events.handlers, key);
          return new Ok(map3(handler, (handler) => {
            return new Handler(handler.prevent_default, handler.stop_propagation, identity3(mapper)(handler.message));
          }));
        } else {
          return error_nil;
        }
      } else {
        let key = path.head;
        let path$1 = $;
        let $1 = has_key(events.children, key);
        if ($1) {
          let child = get2(events.children, key);
          let mapper$1 = compose_mapper(mapper, child.mapper);
          loop$events = child.events;
          loop$path = path$1;
          loop$mapper = mapper$1;
        } else {
          return error_nil;
        }
      }
    }
  }
}
function decode2(cache, path, name, event) {
  let parts = split_subtree_path(path + separator_event + name);
  let $ = get_handler(cache.events, parts, identity3);
  if ($ instanceof Ok) {
    let handler = $[0];
    let $1 = run(event, handler);
    if ($1 instanceof Ok) {
      let handler$1 = $1[0];
      return new DecodedEvent(path, handler$1);
    } else {
      return new DispatchedEvent(path);
    }
  } else {
    return new DispatchedEvent(path);
  }
}
function dispatch(cache, event) {
  let next_dispatched_paths = prepend(event.path, cache.next_dispatched_paths);
  let cache$1 = new Cache(cache.events, cache.vdoms, cache.old_vdoms, cache.dispatched_paths, next_dispatched_paths);
  if (event instanceof DecodedEvent) {
    let handler = event.handler;
    return [cache$1, new Ok(handler)];
  } else {
    return [cache$1, error_nil];
  }
}
function has_dispatched_events(cache, path) {
  return matches(path, cache.dispatched_paths);
}

// build/dev/javascript/lustre/lustre/runtime/server/runtime.mjs
class EffectDispatchedMessage extends CustomType {
  constructor(message) {
    super();
    this.message = message;
  }
}
var Message$isEffectDispatchedMessage = (value) => value instanceof EffectDispatchedMessage;
class EffectEmitEvent extends CustomType {
  constructor(name, data) {
    super();
    this.name = name;
    this.data = data;
  }
}
var Message$isEffectEmitEvent = (value) => value instanceof EffectEmitEvent;
class SystemRequestedShutdown extends CustomType {
}
var Message$SystemRequestedShutdown$const = new SystemRequestedShutdown;
var Message$isSystemRequestedShutdown = (value) => value instanceof SystemRequestedShutdown;

// build/dev/javascript/lustre/lustre/runtime/app.mjs
class App extends CustomType {
  constructor(name, init, update, view, config) {
    super();
    this.name = name;
    this.init = init;
    this.update = update;
    this.view = view;
    this.config = config;
  }
}
class Config2 extends CustomType {
  constructor(open_shadow_root, adopt_styles, delegates_focus, attributes, properties, contexts, is_form_associated, on_form_autofill, on_form_reset, on_form_restore, on_form_disabled, on_connect, on_adopt, on_disconnect) {
    super();
    this.open_shadow_root = open_shadow_root;
    this.adopt_styles = adopt_styles;
    this.delegates_focus = delegates_focus;
    this.attributes = attributes;
    this.properties = properties;
    this.contexts = contexts;
    this.is_form_associated = is_form_associated;
    this.on_form_autofill = on_form_autofill;
    this.on_form_reset = on_form_reset;
    this.on_form_restore = on_form_restore;
    this.on_form_disabled = on_form_disabled;
    this.on_connect = on_connect;
    this.on_adopt = on_adopt;
    this.on_disconnect = on_disconnect;
  }
}
var default_config = /* @__PURE__ */ new Config2(true, true, false, empty_list, empty_list, empty_list, false, Option$None$const, Option$None$const, Option$None$const, Option$None$const, Option$None$const, Option$None$const, Option$None$const);

// build/dev/javascript/lustre/lustre/internals/equals.ffi.mjs
var isEqual2 = (a, b) => {
  if (a === b) {
    return true;
  }
  if (a == null || b == null) {
    return false;
  }
  const type = typeof a;
  if (type !== typeof b) {
    return false;
  }
  if (type !== "object") {
    return false;
  }
  const ctor = a.constructor;
  if (ctor !== b.constructor) {
    return false;
  }
  if (Array.isArray(a)) {
    return areArraysEqual(a, b);
  }
  return areObjectsEqual(a, b);
};
var areArraysEqual = (a, b) => {
  let index = a.length;
  if (index !== b.length) {
    return false;
  }
  while (index--) {
    if (!isEqual2(a[index], b[index])) {
      return false;
    }
  }
  return true;
};
var areObjectsEqual = (a, b) => {
  const properties = Object.keys(a);
  let index = properties.length;
  if (Object.keys(b).length !== index) {
    return false;
  }
  while (index--) {
    const property = properties[index];
    if (!Object.hasOwn(b, property)) {
      return false;
    }
    if (!isEqual2(a[property], b[property])) {
      return false;
    }
  }
  return true;
};

// build/dev/javascript/lustre/lustre/vdom/diff.mjs
class Diff extends CustomType {
  constructor(patch, cache) {
    super();
    this.patch = patch;
    this.cache = cache;
  }
}
class PartialDiff extends CustomType {
  constructor(patch, cache, events) {
    super();
    this.patch = patch;
    this.cache = cache;
    this.events = events;
  }
}

class AttributeChange extends CustomType {
  constructor(added, removed, events) {
    super();
    this.added = added;
    this.removed = removed;
    this.events = events;
  }
}
function diff_attributes(loop$controlled, loop$path, loop$events, loop$old, loop$new, loop$added, loop$removed) {
  while (true) {
    let controlled = loop$controlled;
    let path = loop$path;
    let events = loop$events;
    let old = loop$old;
    let new$ = loop$new;
    let added = loop$added;
    let removed = loop$removed;
    if (old instanceof Empty) {
      if (new$ instanceof Empty) {
        return new AttributeChange(added, removed, events);
      } else {
        let $ = new$.head;
        if ($ instanceof Event2) {
          let next = $;
          let new$1 = new$.tail;
          let name = $.name;
          let handler = $.handler;
          let events$1 = add_event(events, path, name, handler);
          let added$1 = prepend(next, added);
          loop$controlled = controlled;
          loop$path = path;
          loop$events = events$1;
          loop$old = old;
          loop$new = new$1;
          loop$added = added$1;
          loop$removed = removed;
        } else {
          let next = $;
          let new$1 = new$.tail;
          let added$1 = prepend(next, added);
          loop$controlled = controlled;
          loop$path = path;
          loop$events = events;
          loop$old = old;
          loop$new = new$1;
          loop$added = added$1;
          loop$removed = removed;
        }
      }
    } else if (new$ instanceof Empty) {
      let $ = old.head;
      if ($ instanceof Event2) {
        let prev = $;
        let old$1 = old.tail;
        let name = $.name;
        let events$1 = remove_event(events, path, name);
        let removed$1 = prepend(prev, removed);
        loop$controlled = controlled;
        loop$path = path;
        loop$events = events$1;
        loop$old = old$1;
        loop$new = new$;
        loop$added = added;
        loop$removed = removed$1;
      } else {
        let prev = $;
        let old$1 = old.tail;
        let removed$1 = prepend(prev, removed);
        loop$controlled = controlled;
        loop$path = path;
        loop$events = events;
        loop$old = old$1;
        loop$new = new$;
        loop$added = added;
        loop$removed = removed$1;
      }
    } else {
      let prev = old.head;
      let remaining_old = old.tail;
      let next = new$.head;
      let remaining_new = new$.tail;
      let $ = compare3(prev, next);
      if ($ instanceof Lt) {
        if (prev instanceof Event2) {
          let name = prev.name;
          loop$controlled = controlled;
          loop$path = path;
          loop$events = remove_event(events, path, name);
          loop$old = remaining_old;
          loop$new = new$;
          loop$added = added;
          loop$removed = prepend(prev, removed);
        } else {
          loop$controlled = controlled;
          loop$path = path;
          loop$events = events;
          loop$old = remaining_old;
          loop$new = new$;
          loop$added = added;
          loop$removed = prepend(prev, removed);
        }
      } else if ($ instanceof Eq) {
        if (prev instanceof Attribute) {
          if (next instanceof Attribute) {
            let _block;
            let $1 = next.name;
            if ($1 === "value") {
              _block = controlled || prev.value !== next.value;
            } else if ($1 === "checked") {
              _block = controlled || prev.value !== next.value;
            } else if ($1 === "selected") {
              _block = controlled || prev.value !== next.value;
            } else {
              _block = prev.value !== next.value;
            }
            let has_changes = _block;
            let _block$1;
            if (has_changes) {
              _block$1 = prepend(next, added);
            } else {
              _block$1 = added;
            }
            let added$1 = _block$1;
            loop$controlled = controlled;
            loop$path = path;
            loop$events = events;
            loop$old = remaining_old;
            loop$new = remaining_new;
            loop$added = added$1;
            loop$removed = removed;
          } else if (next instanceof Event2) {
            let name = next.name;
            let handler = next.handler;
            loop$controlled = controlled;
            loop$path = path;
            loop$events = add_event(events, path, name, handler);
            loop$old = remaining_old;
            loop$new = remaining_new;
            loop$added = prepend(next, added);
            loop$removed = prepend(prev, removed);
          } else {
            loop$controlled = controlled;
            loop$path = path;
            loop$events = events;
            loop$old = remaining_old;
            loop$new = remaining_new;
            loop$added = prepend(next, added);
            loop$removed = prepend(prev, removed);
          }
        } else if (prev instanceof Property) {
          if (next instanceof Property) {
            let _block;
            let $1 = next.name;
            if ($1 === "scrollLeft") {
              _block = true;
            } else if ($1 === "scrollRight") {
              _block = true;
            } else if ($1 === "value") {
              _block = controlled || !isEqual2(prev.value, next.value);
            } else if ($1 === "checked") {
              _block = controlled || !isEqual2(prev.value, next.value);
            } else if ($1 === "selected") {
              _block = controlled || !isEqual2(prev.value, next.value);
            } else {
              _block = !isEqual2(prev.value, next.value);
            }
            let has_changes = _block;
            let _block$1;
            if (has_changes) {
              _block$1 = prepend(next, added);
            } else {
              _block$1 = added;
            }
            let added$1 = _block$1;
            loop$controlled = controlled;
            loop$path = path;
            loop$events = events;
            loop$old = remaining_old;
            loop$new = remaining_new;
            loop$added = added$1;
            loop$removed = removed;
          } else if (next instanceof Event2) {
            let name = next.name;
            let handler = next.handler;
            loop$controlled = controlled;
            loop$path = path;
            loop$events = add_event(events, path, name, handler);
            loop$old = remaining_old;
            loop$new = remaining_new;
            loop$added = prepend(next, added);
            loop$removed = prepend(prev, removed);
          } else {
            loop$controlled = controlled;
            loop$path = path;
            loop$events = events;
            loop$old = remaining_old;
            loop$new = remaining_new;
            loop$added = prepend(next, added);
            loop$removed = prepend(prev, removed);
          }
        } else if (next instanceof Event2) {
          let name = next.name;
          let handler = next.handler;
          let has_changes = prev.prevent_default.kind !== next.prevent_default.kind || prev.stop_propagation.kind !== next.stop_propagation.kind || prev.debounce !== next.debounce || prev.throttle !== next.throttle;
          let _block;
          if (has_changes) {
            _block = prepend(next, added);
          } else {
            _block = added;
          }
          let added$1 = _block;
          loop$controlled = controlled;
          loop$path = path;
          loop$events = add_event(events, path, name, handler);
          loop$old = remaining_old;
          loop$new = remaining_new;
          loop$added = added$1;
          loop$removed = removed;
        } else {
          let name = prev.name;
          loop$controlled = controlled;
          loop$path = path;
          loop$events = remove_event(events, path, name);
          loop$old = remaining_old;
          loop$new = remaining_new;
          loop$added = prepend(next, added);
          loop$removed = prepend(prev, removed);
        }
      } else if (next instanceof Event2) {
        let name = next.name;
        let handler = next.handler;
        loop$controlled = controlled;
        loop$path = path;
        loop$events = add_event(events, path, name, handler);
        loop$old = old;
        loop$new = remaining_new;
        loop$added = prepend(next, added);
        loop$removed = removed;
      } else {
        loop$controlled = controlled;
        loop$path = path;
        loop$events = events;
        loop$old = old;
        loop$new = remaining_new;
        loop$added = prepend(next, added);
        loop$removed = removed;
      }
    }
  }
}
function is_controlled(cache, namespace, tag, path) {
  if (tag === "input" && namespace === "") {
    return has_dispatched_events(cache, path);
  } else if (tag === "select" && namespace === "") {
    return has_dispatched_events(cache, path);
  } else if (tag === "textarea" && namespace === "") {
    return has_dispatched_events(cache, path);
  } else {
    return false;
  }
}
function do_diff(loop$old, loop$old_keyed, loop$new, loop$new_keyed, loop$moved, loop$moved_offset, loop$removed, loop$node_index, loop$patch_index, loop$changes, loop$children, loop$path, loop$cache, loop$events) {
  while (true) {
    let old = loop$old;
    let old_keyed = loop$old_keyed;
    let new$ = loop$new;
    let new_keyed = loop$new_keyed;
    let moved = loop$moved;
    let moved_offset = loop$moved_offset;
    let removed = loop$removed;
    let node_index = loop$node_index;
    let patch_index = loop$patch_index;
    let changes = loop$changes;
    let children = loop$children;
    let path = loop$path;
    let cache = loop$cache;
    let events = loop$events;
    if (old instanceof Empty) {
      if (new$ instanceof Empty) {
        let _block;
        let $ = is_browser();
        if (changes instanceof Empty) {
          if (children instanceof Empty) {
            _block = new$3(patch_index, removed, changes, children);
          } else if (!$ && children.tail instanceof Empty && removed === 0) {
            let child = children.head;
            _block = add_parent(child, patch_index);
          } else {
            _block = new$3(patch_index, removed, changes, children);
          }
        } else {
          _block = new$3(patch_index, removed, changes, children);
        }
        let patch = _block;
        return new PartialDiff(patch, cache, events);
      } else {
        let $ = add_children(cache, events, path, node_index, new$);
        let cache$1 = $[0];
        let events$1 = $[1];
        let insert = insert3(new$, node_index - moved_offset);
        let changes$1 = prepend(insert, changes);
        let patch = new$3(patch_index, removed, changes$1, children);
        return new PartialDiff(patch, cache$1, events$1);
      }
    } else if (new$ instanceof Empty) {
      let prev = old.head;
      let old$1 = old.tail;
      let $ = prev.key === "" || !has_key(moved, prev.key);
      if ($) {
        let events$1 = remove_child(cache, events, path, node_index, prev);
        loop$old = old$1;
        loop$old_keyed = old_keyed;
        loop$new = new$;
        loop$new_keyed = new_keyed;
        loop$moved = moved;
        loop$moved_offset = moved_offset;
        loop$removed = removed + 1;
        loop$node_index = node_index;
        loop$patch_index = patch_index;
        loop$changes = changes;
        loop$children = children;
        loop$path = path;
        loop$cache = cache;
        loop$events = events$1;
      } else {
        loop$old = old$1;
        loop$old_keyed = old_keyed;
        loop$new = new$;
        loop$new_keyed = new_keyed;
        loop$moved = moved;
        loop$moved_offset = moved_offset;
        loop$removed = removed;
        loop$node_index = node_index;
        loop$patch_index = patch_index;
        loop$changes = changes;
        loop$children = children;
        loop$path = path;
        loop$cache = cache;
        loop$events = events;
      }
    } else {
      let prev = old.head;
      let next = new$.head;
      if (prev.key !== next.key) {
        let old_remaining = old.tail;
        let new_remaining = new$.tail;
        let next_did_exist = has_key(old_keyed, next.key);
        let prev_does_exist = has_key(new_keyed, prev.key);
        if (prev_does_exist) {
          if (next_did_exist) {
            let $ = has_key(moved, prev.key);
            if ($) {
              loop$old = old_remaining;
              loop$old_keyed = old_keyed;
              loop$new = new$;
              loop$new_keyed = new_keyed;
              loop$moved = moved;
              loop$moved_offset = moved_offset - 1;
              loop$removed = removed;
              loop$node_index = node_index;
              loop$patch_index = patch_index;
              loop$changes = changes;
              loop$children = children;
              loop$path = path;
              loop$cache = cache;
              loop$events = events;
            } else {
              let match = get2(old_keyed, next.key);
              let before = node_index - moved_offset;
              let changes$1 = prepend(move(next.key, before), changes);
              let moved$1 = insert2(moved, next.key, undefined);
              loop$old = prepend(match, old);
              loop$old_keyed = old_keyed;
              loop$new = new$;
              loop$new_keyed = new_keyed;
              loop$moved = moved$1;
              loop$moved_offset = moved_offset + 1;
              loop$removed = removed;
              loop$node_index = node_index;
              loop$patch_index = patch_index;
              loop$changes = changes$1;
              loop$children = children;
              loop$path = path;
              loop$cache = cache;
              loop$events = events;
            }
          } else {
            let before = node_index - moved_offset;
            let $ = add_child(cache, events, path, node_index, next);
            let cache$1 = $[0];
            let events$1 = $[1];
            let insert = insert3(singleton_list(next), before);
            let changes$1 = prepend(insert, changes);
            loop$old = old;
            loop$old_keyed = old_keyed;
            loop$new = new_remaining;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset + 1;
            loop$removed = removed;
            loop$node_index = node_index + 1;
            loop$patch_index = patch_index;
            loop$changes = changes$1;
            loop$children = children;
            loop$path = path;
            loop$cache = cache$1;
            loop$events = events$1;
          }
        } else if (next_did_exist) {
          let index = node_index - moved_offset;
          let changes$1 = prepend(remove2(index), changes);
          let events$1 = remove_child(cache, events, path, node_index, prev);
          loop$old = old_remaining;
          loop$old_keyed = old_keyed;
          loop$new = new$;
          loop$new_keyed = new_keyed;
          loop$moved = moved;
          loop$moved_offset = moved_offset - 1;
          loop$removed = removed;
          loop$node_index = node_index;
          loop$patch_index = patch_index;
          loop$changes = changes$1;
          loop$children = children;
          loop$path = path;
          loop$cache = cache;
          loop$events = events$1;
        } else {
          let change = replace2(node_index - moved_offset, next);
          let $ = replace_child(cache, events, path, node_index, prev, next);
          let cache$1 = $[0];
          let events$1 = $[1];
          loop$old = old_remaining;
          loop$old_keyed = old_keyed;
          loop$new = new_remaining;
          loop$new_keyed = new_keyed;
          loop$moved = moved;
          loop$moved_offset = moved_offset;
          loop$removed = removed;
          loop$node_index = node_index + 1;
          loop$patch_index = patch_index;
          loop$changes = prepend(change, changes);
          loop$children = children;
          loop$path = path;
          loop$cache = cache$1;
          loop$events = events$1;
        }
      } else {
        let $ = old.head;
        if ($ instanceof Fragment) {
          let $1 = new$.head;
          if ($1 instanceof Fragment) {
            let prev$1 = $;
            let old$1 = old.tail;
            let next$1 = $1;
            let new$1 = new$.tail;
            let $2 = do_diff(prev$1.children, prev$1.keyed_children, next$1.children, next$1.keyed_children, empty2(), 0, 0, 0, node_index, empty_list, empty_list, add3(path, node_index, next$1.key), cache, events);
            let patch = $2.patch;
            let cache$1 = $2.cache;
            let events$1 = $2.events;
            let _block;
            if (patch.changes instanceof Empty && patch.children instanceof Empty && patch.removed === 0) {
              _block = children;
            } else {
              _block = prepend(patch, children);
            }
            let children$1 = _block;
            loop$old = old$1;
            loop$old_keyed = old_keyed;
            loop$new = new$1;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset;
            loop$removed = removed;
            loop$node_index = node_index + 1;
            loop$patch_index = patch_index;
            loop$changes = changes;
            loop$children = children$1;
            loop$path = path;
            loop$cache = cache$1;
            loop$events = events$1;
          } else {
            let prev$1 = $;
            let old_remaining$1 = old.tail;
            let next$1 = $1;
            let new_remaining$1 = new$.tail;
            let change = replace2(node_index - moved_offset, next$1);
            let $2 = replace_child(cache, events, path, node_index, prev$1, next$1);
            let cache$1 = $2[0];
            let events$1 = $2[1];
            loop$old = old_remaining$1;
            loop$old_keyed = old_keyed;
            loop$new = new_remaining$1;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset;
            loop$removed = removed;
            loop$node_index = node_index + 1;
            loop$patch_index = patch_index;
            loop$changes = prepend(change, changes);
            loop$children = children;
            loop$path = path;
            loop$cache = cache$1;
            loop$events = events$1;
          }
        } else if ($ instanceof Element2) {
          let $1 = new$.head;
          if ($1 instanceof Element2) {
            let prev$1 = $;
            let next$1 = $1;
            if (prev$1.namespace === next$1.namespace && prev$1.tag === next$1.tag) {
              let old$1 = old.tail;
              let new$1 = new$.tail;
              let child_path = add3(path, node_index, next$1.key);
              let controlled = is_controlled(cache, next$1.namespace, next$1.tag, child_path);
              let $2 = diff_attributes(controlled, child_path, events, prev$1.attributes, next$1.attributes, empty_list, empty_list);
              let added_attrs = $2.added;
              let removed_attrs = $2.removed;
              let events$1 = $2.events;
              let _block;
              if (added_attrs instanceof Empty && removed_attrs instanceof Empty) {
                _block = empty_list;
              } else {
                _block = singleton_list(update(added_attrs, removed_attrs));
              }
              let initial_child_changes = _block;
              let $3 = do_diff(prev$1.children, prev$1.keyed_children, next$1.children, next$1.keyed_children, empty2(), 0, 0, 0, node_index, initial_child_changes, empty_list, child_path, cache, events$1);
              let patch = $3.patch;
              let cache$1 = $3.cache;
              let events$2 = $3.events;
              let _block$1;
              if (patch.changes instanceof Empty && patch.children instanceof Empty && patch.removed === 0) {
                _block$1 = children;
              } else {
                _block$1 = prepend(patch, children);
              }
              let children$1 = _block$1;
              loop$old = old$1;
              loop$old_keyed = old_keyed;
              loop$new = new$1;
              loop$new_keyed = new_keyed;
              loop$moved = moved;
              loop$moved_offset = moved_offset;
              loop$removed = removed;
              loop$node_index = node_index + 1;
              loop$patch_index = patch_index;
              loop$changes = changes;
              loop$children = children$1;
              loop$path = path;
              loop$cache = cache$1;
              loop$events = events$2;
            } else {
              let prev$2 = $;
              let old_remaining$1 = old.tail;
              let next$2 = $1;
              let new_remaining$1 = new$.tail;
              let change = replace2(node_index - moved_offset, next$2);
              let $4 = replace_child(cache, events, path, node_index, prev$2, next$2);
              let cache$2 = $4[0];
              let events$3 = $4[1];
              loop$old = old_remaining$1;
              loop$old_keyed = old_keyed;
              loop$new = new_remaining$1;
              loop$new_keyed = new_keyed;
              loop$moved = moved;
              loop$moved_offset = moved_offset;
              loop$removed = removed;
              loop$node_index = node_index + 1;
              loop$patch_index = patch_index;
              loop$changes = prepend(change, changes);
              loop$children = children;
              loop$path = path;
              loop$cache = cache$2;
              loop$events = events$3;
            }
          } else {
            let prev$1 = $;
            let old_remaining$1 = old.tail;
            let next$1 = $1;
            let new_remaining$1 = new$.tail;
            let change = replace2(node_index - moved_offset, next$1);
            let $2 = replace_child(cache, events, path, node_index, prev$1, next$1);
            let cache$1 = $2[0];
            let events$1 = $2[1];
            loop$old = old_remaining$1;
            loop$old_keyed = old_keyed;
            loop$new = new_remaining$1;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset;
            loop$removed = removed;
            loop$node_index = node_index + 1;
            loop$patch_index = patch_index;
            loop$changes = prepend(change, changes);
            loop$children = children;
            loop$path = path;
            loop$cache = cache$1;
            loop$events = events$1;
          }
        } else if ($ instanceof Text) {
          let $1 = new$.head;
          if ($1 instanceof Text) {
            let prev$1 = $;
            let next$1 = $1;
            if (prev$1.content === next$1.content) {
              let old$1 = old.tail;
              let new$1 = new$.tail;
              loop$old = old$1;
              loop$old_keyed = old_keyed;
              loop$new = new$1;
              loop$new_keyed = new_keyed;
              loop$moved = moved;
              loop$moved_offset = moved_offset;
              loop$removed = removed;
              loop$node_index = node_index + 1;
              loop$patch_index = patch_index;
              loop$changes = changes;
              loop$children = children;
              loop$path = path;
              loop$cache = cache;
              loop$events = events;
            } else {
              let old$2 = old.tail;
              let next$2 = $1;
              let new$2 = new$.tail;
              let child = new$3(node_index, 0, singleton_list(replace_text(next$2.content)), empty_list);
              loop$old = old$2;
              loop$old_keyed = old_keyed;
              loop$new = new$2;
              loop$new_keyed = new_keyed;
              loop$moved = moved;
              loop$moved_offset = moved_offset;
              loop$removed = removed;
              loop$node_index = node_index + 1;
              loop$patch_index = patch_index;
              loop$changes = changes;
              loop$children = prepend(child, children);
              loop$path = path;
              loop$cache = cache;
              loop$events = events;
            }
          } else {
            let prev$1 = $;
            let old_remaining$1 = old.tail;
            let next$1 = $1;
            let new_remaining$1 = new$.tail;
            let change = replace2(node_index - moved_offset, next$1);
            let $2 = replace_child(cache, events, path, node_index, prev$1, next$1);
            let cache$1 = $2[0];
            let events$1 = $2[1];
            loop$old = old_remaining$1;
            loop$old_keyed = old_keyed;
            loop$new = new_remaining$1;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset;
            loop$removed = removed;
            loop$node_index = node_index + 1;
            loop$patch_index = patch_index;
            loop$changes = prepend(change, changes);
            loop$children = children;
            loop$path = path;
            loop$cache = cache$1;
            loop$events = events$1;
          }
        } else if ($ instanceof UnsafeInnerHtml) {
          let $1 = new$.head;
          if ($1 instanceof UnsafeInnerHtml) {
            let prev$1 = $;
            let old$1 = old.tail;
            let next$1 = $1;
            let new$1 = new$.tail;
            let child_path = add3(path, node_index, next$1.key);
            let $2 = diff_attributes(false, child_path, events, prev$1.attributes, next$1.attributes, empty_list, empty_list);
            let added_attrs = $2.added;
            let removed_attrs = $2.removed;
            let events$1 = $2.events;
            let _block;
            if (added_attrs instanceof Empty && removed_attrs instanceof Empty) {
              _block = empty_list;
            } else {
              _block = singleton_list(update(added_attrs, removed_attrs));
            }
            let child_changes = _block;
            let _block$1;
            let $3 = prev$1.inner_html === next$1.inner_html;
            if ($3) {
              _block$1 = child_changes;
            } else {
              _block$1 = prepend(replace_inner_html(next$1.inner_html), child_changes);
            }
            let child_changes$1 = _block$1;
            let _block$2;
            if (child_changes$1 instanceof Empty) {
              _block$2 = children;
            } else {
              _block$2 = prepend(new$3(node_index, 0, child_changes$1, empty_list), children);
            }
            let children$1 = _block$2;
            loop$old = old$1;
            loop$old_keyed = old_keyed;
            loop$new = new$1;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset;
            loop$removed = removed;
            loop$node_index = node_index + 1;
            loop$patch_index = patch_index;
            loop$changes = changes;
            loop$children = children$1;
            loop$path = path;
            loop$cache = cache;
            loop$events = events$1;
          } else {
            let prev$1 = $;
            let old_remaining$1 = old.tail;
            let next$1 = $1;
            let new_remaining$1 = new$.tail;
            let change = replace2(node_index - moved_offset, next$1);
            let $2 = replace_child(cache, events, path, node_index, prev$1, next$1);
            let cache$1 = $2[0];
            let events$1 = $2[1];
            loop$old = old_remaining$1;
            loop$old_keyed = old_keyed;
            loop$new = new_remaining$1;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset;
            loop$removed = removed;
            loop$node_index = node_index + 1;
            loop$patch_index = patch_index;
            loop$changes = prepend(change, changes);
            loop$children = children;
            loop$path = path;
            loop$cache = cache$1;
            loop$events = events$1;
          }
        } else if ($ instanceof Map2) {
          let $1 = new$.head;
          if ($1 instanceof Map2) {
            let prev$1 = $;
            let old$1 = old.tail;
            let next$1 = $1;
            let new$1 = new$.tail;
            let child_path = add3(path, node_index, next$1.key);
            let child_key = child(child_path);
            let $2 = do_diff(singleton_list(prev$1.child), empty2(), singleton_list(next$1.child), empty2(), empty2(), 0, 0, 0, node_index, empty_list, empty_list, subtree(child_path), cache, get_subtree(events, child_key, prev$1.mapper));
            let patch = $2.patch;
            let cache$1 = $2.cache;
            let child_events = $2.events;
            let events$1 = update_subtree(events, child_key, next$1.mapper, child_events);
            let _block;
            if (patch.changes instanceof Empty && patch.children instanceof Empty && patch.removed === 0) {
              _block = children;
            } else {
              _block = prepend(patch, children);
            }
            let children$1 = _block;
            loop$old = old$1;
            loop$old_keyed = old_keyed;
            loop$new = new$1;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset;
            loop$removed = removed;
            loop$node_index = node_index + 1;
            loop$patch_index = patch_index;
            loop$changes = changes;
            loop$children = children$1;
            loop$path = path;
            loop$cache = cache$1;
            loop$events = events$1;
          } else {
            let prev$1 = $;
            let old_remaining$1 = old.tail;
            let next$1 = $1;
            let new_remaining$1 = new$.tail;
            let change = replace2(node_index - moved_offset, next$1);
            let $2 = replace_child(cache, events, path, node_index, prev$1, next$1);
            let cache$1 = $2[0];
            let events$1 = $2[1];
            loop$old = old_remaining$1;
            loop$old_keyed = old_keyed;
            loop$new = new_remaining$1;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset;
            loop$removed = removed;
            loop$node_index = node_index + 1;
            loop$patch_index = patch_index;
            loop$changes = prepend(change, changes);
            loop$children = children;
            loop$path = path;
            loop$cache = cache$1;
            loop$events = events$1;
          }
        } else {
          let $1 = new$.head;
          if ($1 instanceof Memo) {
            let prev$1 = $;
            let old$1 = old.tail;
            let next$1 = $1;
            let new$1 = new$.tail;
            let $2 = equal_lists(prev$1.dependencies, next$1.dependencies);
            if ($2) {
              let cache$1 = keep_memo(cache, prev$1.view, next$1.view);
              loop$old = old$1;
              loop$old_keyed = old_keyed;
              loop$new = new$1;
              loop$new_keyed = new_keyed;
              loop$moved = moved;
              loop$moved_offset = moved_offset;
              loop$removed = removed;
              loop$node_index = node_index + 1;
              loop$patch_index = patch_index;
              loop$changes = changes;
              loop$children = children;
              loop$path = path;
              loop$cache = cache$1;
              loop$events = events;
            } else {
              let prev_node = get_old_memo(cache, prev$1.view, prev$1.view);
              let next_node = next$1.view();
              let cache$1 = add_memo(cache, next$1.view, next_node);
              loop$old = prepend(prev_node, old$1);
              loop$old_keyed = old_keyed;
              loop$new = prepend(next_node, new$1);
              loop$new_keyed = new_keyed;
              loop$moved = moved;
              loop$moved_offset = moved_offset;
              loop$removed = removed;
              loop$node_index = node_index;
              loop$patch_index = patch_index;
              loop$changes = changes;
              loop$children = children;
              loop$path = path;
              loop$cache = cache$1;
              loop$events = events;
            }
          } else {
            let prev$1 = $;
            let old_remaining$1 = old.tail;
            let next$1 = $1;
            let new_remaining$1 = new$.tail;
            let change = replace2(node_index - moved_offset, next$1);
            let $2 = replace_child(cache, events, path, node_index, prev$1, next$1);
            let cache$1 = $2[0];
            let events$1 = $2[1];
            loop$old = old_remaining$1;
            loop$old_keyed = old_keyed;
            loop$new = new_remaining$1;
            loop$new_keyed = new_keyed;
            loop$moved = moved;
            loop$moved_offset = moved_offset;
            loop$removed = removed;
            loop$node_index = node_index + 1;
            loop$patch_index = patch_index;
            loop$changes = prepend(change, changes);
            loop$children = children;
            loop$path = path;
            loop$cache = cache$1;
            loop$events = events$1;
          }
        }
      }
    }
  }
}
function diff(cache, old, new$) {
  let cache$1 = tick(cache);
  let $ = do_diff(singleton_list(old), empty2(), singleton_list(new$), empty2(), empty2(), 0, 0, 0, 0, empty_list, empty_list, root, cache$1, events(cache$1));
  let patch = $.patch;
  let cache$2 = $.cache;
  let events2 = $.events;
  return new Diff(patch, update_events(cache$2, events2));
}

// build/dev/javascript/lustre/lustre/internals/list.ffi.mjs
var iterate = (list, callback) => {
  if (Array.isArray(list)) {
    for (let i = 0;i < list.length; i++) {
      callback(list[i]);
    }
  } else if (list) {
    for (list;List$NonEmpty$rest(list); list = List$NonEmpty$rest(list)) {
      callback(List$NonEmpty$first(list));
    }
  }
};
var append4 = (a, b) => {
  if (!List$NonEmpty$rest(a)) {
    return b;
  } else if (!List$NonEmpty$rest(b)) {
    return a;
  } else {
    return append(a, b);
  }
};

// build/dev/javascript/lustre/lustre/internals/constants.ffi.mjs
var NAMESPACE_HTML = "http://www.w3.org/1999/xhtml";
var ELEMENT_NODE = 1;
var TEXT_NODE = 3;
var COMMENT_NODE = 8;
var SUPPORTS_MOVE_BEFORE = !!globalThis.HTMLElement?.prototype?.moveBefore;

// build/dev/javascript/lustre/lustre/vdom/reconciler.ffi.mjs
var setTimeout = globalThis.setTimeout;
var clearTimeout = globalThis.clearTimeout;
var createElementNS = (ns, name) => globalThis.document.createElementNS(ns, name);
var createTextNode = (data) => globalThis.document.createTextNode(data);
var createComment = (data) => globalThis.document.createComment(data);
var createDocumentFragment = () => globalThis.document.createDocumentFragment();
var insertBefore = (parent, node, reference) => parent.insertBefore(node, reference);
var moveBefore = SUPPORTS_MOVE_BEFORE ? (parent, node, reference) => parent.moveBefore(node, reference) : insertBefore;
var removeChild = (parent, child) => parent.removeChild(child);
var getAttribute = (node, name) => node.getAttribute(name);
var setAttribute = (node, name, value) => node.setAttribute(name, value);
var removeAttribute = (node, name) => node.removeAttribute(name);
var addEventListener = (node, name, handler, options) => node.addEventListener(name, handler, options);
var removeEventListener = (node, name, handler) => node.removeEventListener(name, handler);
var setInnerHtml = (node, innerHtml) => node.innerHTML = innerHtml;
var setData = (node, data) => node.data = data;
var meta = Symbol("lustre");

class MetadataNode {
  constructor(kind, parent, node, key) {
    this.kind = kind;
    this.key = key;
    this.parent = parent;
    this.children = [];
    this.node = node;
    this.endNode = null;
    this.handlers = new Map;
    this.throttles = new Map;
    this.debouncers = new Map;
  }
  get isVirtual() {
    return this.kind === fragment_kind || this.kind === map_kind;
  }
  get parentNode() {
    return this.isVirtual ? this.node.parentNode : this.node;
  }
}
var insertMetadataChild = (kind, parent, node, index, key) => {
  const child = new MetadataNode(kind, parent, node, key);
  node[meta] = child;
  parent?.children.splice(index, 0, child);
  return child;
};
var getPath = (node) => {
  let path = "";
  for (let current = node[meta];current.parent; current = current.parent) {
    const separator = current.parent && current.parent.kind === map_kind ? separator_subtree : separator_element;
    if (current.key) {
      path = `${separator}${current.key}${path}`;
    } else {
      const index = current.parent.children.indexOf(current);
      path = `${separator}${index}${path}`;
    }
  }
  return path.slice(1);
};

class Reconciler {
  #root = null;
  #decodeEvent;
  #dispatch;
  #debug = false;
  constructor(root, decodeEvent, dispatch, { debug = false } = {}) {
    this.#root = root;
    this.#decodeEvent = decodeEvent;
    this.#dispatch = dispatch;
    this.#debug = debug;
  }
  mount(vdom) {
    insertMetadataChild(element_kind, null, this.#root, 0, null);
    this.#insertChild(this.#root, null, this.#root[meta], 0, vdom);
  }
  push(patch, memos = null) {
    this.#memos = memos;
    this.#stack.push({ node: this.#root[meta], patch });
    this.#reconcile();
  }
  #memos;
  #stack = [];
  #reconcile() {
    const stack = this.#stack;
    while (stack.length) {
      let { node, patch } = stack.pop();
      const { path, changes, removed, children: childPatches } = patch;
      iterate(path, (index) => {
        node = node.children[index];
      });
      const { children: childNodes } = node;
      iterate(changes, (change) => this.#patch(node, change));
      if (removed) {
        this.#removeChildren(node, childNodes.length - removed, removed);
      }
      iterate(childPatches, (childPatch) => {
        const child = childNodes[childPatch.index | 0];
        this.#stack.push({ node: child, patch: childPatch });
      });
    }
  }
  #patch(node, change) {
    switch (change.kind) {
      case replace_text_kind:
        this.#replaceText(node, change);
        break;
      case replace_inner_html_kind:
        this.#replaceInnerHtml(node, change);
        break;
      case update_kind:
        this.#update(node, change);
        break;
      case move_kind:
        this.#move(node, change);
        break;
      case remove_kind:
        this.#remove(node, change);
        break;
      case replace_kind:
        this.#replace(node, change);
        break;
      case insert_kind:
        this.#insert(node, change);
        break;
    }
  }
  #insert(parent, { children, before }) {
    const fragment = createDocumentFragment();
    const beforeEl = this.#getReference(parent, before);
    this.#insertChildren(fragment, null, parent, before | 0, children);
    insertBefore(parent.parentNode, fragment, beforeEl);
  }
  #replace(parent, { index, with: child }) {
    this.#removeChildren(parent, index | 0, 1);
    const beforeEl = this.#getReference(parent, index);
    this.#insertChild(parent.parentNode, beforeEl, parent, index | 0, child);
  }
  #getReference(node, index) {
    index = index | 0;
    const { children } = node;
    const childCount = children.length;
    if (index < childCount)
      return children[index].node;
    if (node.endNode)
      return node.endNode;
    if (!node.isVirtual)
      return null;
    while (node.isVirtual && node.children.length) {
      if (node.endNode)
        return node.endNode.nextSibling;
      node = node.children[node.children.length - 1];
    }
    return node.node.nextSibling;
  }
  #move(parent, { key, before }) {
    before = before | 0;
    const { children, parentNode } = parent;
    const beforeEl = children[before].node;
    let prev = children[before];
    for (let i = before + 1;i < children.length; ++i) {
      const next = children[i];
      children[i] = prev;
      prev = next;
      if (next.key === key) {
        children[before] = next;
        break;
      }
    }
    this.#moveChild(parentNode, prev, beforeEl);
  }
  #moveChildren(domParent, children, beforeEl) {
    for (let i = 0;i < children.length; ++i) {
      this.#moveChild(domParent, children[i], beforeEl);
    }
  }
  #moveChild(domParent, child, beforeEl) {
    moveBefore(domParent, child.node, beforeEl);
    if (child.isVirtual) {
      this.#moveChildren(domParent, child.children, beforeEl);
    }
    if (child.endNode) {
      moveBefore(domParent, child.endNode, beforeEl);
    }
  }
  #remove(parent, { index }) {
    this.#removeChildren(parent, index, 1);
  }
  #removeChildren(parent, index, count) {
    const { children, parentNode } = parent;
    const deleted = children.splice(index, count);
    for (let i = 0;i < deleted.length; ++i) {
      const child = deleted[i];
      const { node, endNode, isVirtual, children: nestedChildren } = child;
      removeChild(parentNode, node);
      if (endNode) {
        removeChild(parentNode, endNode);
      }
      this.#removeDebouncers(child);
      if (isVirtual) {
        deleted.push(...nestedChildren);
      }
    }
  }
  #removeDebouncers(node) {
    const { debouncers, children } = node;
    for (const { timeout } of debouncers.values()) {
      if (timeout) {
        clearTimeout(timeout);
      }
    }
    debouncers.clear();
    iterate(children, (child) => this.#removeDebouncers(child));
  }
  #update({ node, handlers, throttles, debouncers }, { added, removed }) {
    iterate(removed, ({ name }) => {
      if (handlers.delete(name)) {
        removeEventListener(node, name, handleEvent);
        this.#updateDebounceThrottle(throttles, name, 0);
        this.#updateDebounceThrottle(debouncers, name, 0);
      } else {
        removeAttribute(node, name);
        SYNCED_ATTRIBUTES[name]?.removed?.(node, name);
      }
    });
    iterate(added, (attribute) => this.#createAttribute(node, attribute));
  }
  #replaceText({ node }, { content }) {
    setData(node, content ?? "");
  }
  #replaceInnerHtml({ node }, { inner_html }) {
    setInnerHtml(node, inner_html ?? "");
  }
  #insertChildren(domParent, beforeEl, metaParent, index, children) {
    iterate(children, (child) => this.#insertChild(domParent, beforeEl, metaParent, index++, child));
  }
  #insertChild(domParent, beforeEl, metaParent, index, vnode) {
    switch (vnode.kind) {
      case element_kind: {
        const node = this.#createElement(metaParent, index, vnode);
        this.#insertChildren(node, null, node[meta], 0, vnode.children);
        insertBefore(domParent, node, beforeEl);
        break;
      }
      case text_kind: {
        const node = this.#createTextNode(metaParent, index, vnode);
        insertBefore(domParent, node, beforeEl);
        break;
      }
      case fragment_kind: {
        const marker = "lustre:fragment";
        const head = this.#createHead(marker, metaParent, index, vnode);
        insertBefore(domParent, head, beforeEl);
        this.#insertChildren(domParent, beforeEl, head[meta], 0, vnode.children);
        if (this.#debug) {
          head[meta].endNode = createComment(` /${marker} `);
          insertBefore(domParent, head[meta].endNode, beforeEl);
        }
        break;
      }
      case unsafe_inner_html_kind: {
        const node = this.#createElement(metaParent, index, vnode);
        this.#replaceInnerHtml({ node }, vnode);
        insertBefore(domParent, node, beforeEl);
        break;
      }
      case map_kind: {
        const head = this.#createHead("lustre:map", metaParent, index, vnode);
        insertBefore(domParent, head, beforeEl);
        this.#insertChild(domParent, beforeEl, head[meta], 0, vnode.child);
        break;
      }
      case memo_kind: {
        const child = this.#memos?.get(vnode.view) ?? vnode.view();
        this.#insertChild(domParent, beforeEl, metaParent, index, child);
        break;
      }
    }
  }
  #createElement(parent, index, { kind, key, tag, namespace, attributes }) {
    const node = createElementNS(namespace || NAMESPACE_HTML, tag);
    insertMetadataChild(kind, parent, node, index, key);
    if (this.#debug && key) {
      setAttribute(node, "data-lustre-key", key);
    }
    iterate(attributes, (attribute) => this.#createAttribute(node, attribute));
    return node;
  }
  #createTextNode(parent, index, { kind, key, content }) {
    const node = createTextNode(content ?? "");
    insertMetadataChild(kind, parent, node, index, key);
    return node;
  }
  #createHead(marker, parent, index, { kind, key }) {
    const node = this.#debug ? createComment(markerComment(marker, key)) : createTextNode("");
    insertMetadataChild(kind, parent, node, index, key);
    return node;
  }
  #createAttribute(node, attribute) {
    const { debouncers, handlers, throttles } = node[meta];
    const {
      kind,
      name,
      value,
      prevent_default: prevent,
      debounce: debounceDelay,
      throttle: throttleDelay
    } = attribute;
    switch (kind) {
      case attribute_kind: {
        const valueOrDefault = value ?? "";
        if (name === "virtual:defaultValue") {
          node.defaultValue = valueOrDefault;
          return;
        } else if (name === "virtual:defaultChecked") {
          node.defaultChecked = true;
          return;
        } else if (name === "virtual:defaultSelected") {
          node.defaultSelected = true;
          return;
        }
        if (valueOrDefault !== getAttribute(node, name)) {
          setAttribute(node, name, valueOrDefault);
        }
        SYNCED_ATTRIBUTES[name]?.added?.(node, valueOrDefault);
        break;
      }
      case property_kind:
        node[name] = value;
        break;
      case event_kind: {
        if (handlers.has(name)) {
          removeEventListener(node, name, handleEvent);
        }
        const passive = prevent.kind === never_kind;
        addEventListener(node, name, handleEvent, { passive });
        this.#updateDebounceThrottle(throttles, name, throttleDelay);
        this.#updateDebounceThrottle(debouncers, name, debounceDelay);
        handlers.set(name, (event) => this.#handleEvent(attribute, event));
        break;
      }
    }
  }
  #updateDebounceThrottle(map, name, delay) {
    const debounceOrThrottle = map.get(name);
    if (delay > 0) {
      if (debounceOrThrottle) {
        debounceOrThrottle.delay = delay;
      } else {
        map.set(name, { delay });
      }
    } else if (debounceOrThrottle) {
      const { timeout } = debounceOrThrottle;
      if (timeout) {
        clearTimeout(timeout);
      }
      map.delete(name);
    }
  }
  #handleEvent(attribute, event) {
    const { currentTarget, type } = event;
    const { debouncers, throttles } = currentTarget[meta];
    const path = getPath(currentTarget);
    const {
      prevent_default: prevent,
      stop_propagation: stop,
      include
    } = attribute;
    if (prevent.kind === always_kind)
      event.preventDefault();
    if (stop.kind === always_kind)
      event.stopPropagation();
    if (type === "submit") {
      event.detail ??= {};
      event.detail.formData = [
        ...new FormData(event.target, event.submitter).entries()
      ];
    }
    const data = this.#decodeEvent(event, path, type, include);
    const throttle = throttles.get(type);
    if (throttle) {
      const now = Date.now();
      const last = throttle.last || 0;
      if (now > last + throttle.delay) {
        throttle.last = now;
        throttle.lastEvent = event;
        this.#dispatch(event, data);
      }
    }
    const debounce = debouncers.get(type);
    if (debounce) {
      clearTimeout(debounce.timeout);
      debounce.timeout = setTimeout(() => {
        if (event === throttles.get(type)?.lastEvent)
          return;
        this.#dispatch(event, data);
      }, debounce.delay);
    }
    if (!throttle && !debounce) {
      this.#dispatch(event, data);
    }
  }
}
var markerComment = (marker, key) => {
  if (key) {
    return ` ${marker} key="${escape(key)}" `;
  } else {
    return ` ${marker} `;
  }
};
var handleEvent = (event) => {
  const { currentTarget, type } = event;
  const handler = currentTarget[meta].handlers.get(type);
  handler(event);
};
var syncedBooleanAttribute = (name) => {
  return {
    added(node) {
      node[name] = true;
    },
    removed(node) {
      node[name] = false;
    }
  };
};
var syncedAttribute = (name) => {
  return {
    added(node, value) {
      node[name] = value;
    }
  };
};
var SYNCED_ATTRIBUTES = {
  checked: syncedBooleanAttribute("checked"),
  selected: syncedBooleanAttribute("selected"),
  value: syncedAttribute("value"),
  autofocus: {
    added(node) {
      queueMicrotask(() => {
        node.focus?.();
      });
    }
  },
  autoplay: {
    added(node) {
      try {
        node.play?.();
      } catch (e) {
        console.error(e);
      }
    }
  }
};

// build/dev/javascript/lustre/lustre/element/keyed.mjs
function do_extract_keyed_children(loop$key_children_pairs, loop$keyed_children, loop$children) {
  while (true) {
    let key_children_pairs = loop$key_children_pairs;
    let keyed_children = loop$keyed_children;
    let children = loop$children;
    if (key_children_pairs instanceof Empty) {
      return [keyed_children, reverse(children)];
    } else {
      let rest = key_children_pairs.tail;
      let key = key_children_pairs.head[0];
      let element$1 = key_children_pairs.head[1];
      let keyed_element = to_keyed(key, element$1);
      let _block;
      if (key === "") {
        _block = keyed_children;
      } else {
        _block = insert2(keyed_children, key, keyed_element);
      }
      let keyed_children$1 = _block;
      let children$1 = prepend(keyed_element, children);
      loop$key_children_pairs = rest;
      loop$keyed_children = keyed_children$1;
      loop$children = children$1;
    }
  }
}
function extract_keyed_children(children) {
  return do_extract_keyed_children(children, empty2(), empty_list);
}
function element3(tag, attributes, children) {
  let $ = extract_keyed_children(children);
  let keyed_children = $[0];
  let children$1 = $[1];
  return element("", "", tag, attributes, children$1, keyed_children, false, is_void_html_element(tag, ""));
}
function namespaced2(namespace, tag, attributes, children) {
  let $ = extract_keyed_children(children);
  let keyed_children = $[0];
  let children$1 = $[1];
  return element("", namespace, tag, attributes, children$1, keyed_children, false, is_void_html_element(tag, namespace));
}
function fragment2(children) {
  let $ = extract_keyed_children(children);
  let keyed_children = $[0];
  let children$1 = $[1];
  return fragment("", children$1, keyed_children);
}

// build/dev/javascript/lustre/lustre/vdom/virtualise.ffi.mjs
var virtualise = (root) => {
  const rootMeta = insertMetadataChild(element_kind, null, root, 0, null);
  const { children } = virtualiseChildren(rootMeta, root, root.firstChild);
  if (children.length > 1) {
    const rootNodeMeta = insertMetadataChild(element_kind, null, root, 0, null);
    rootMeta.kind = fragment_kind;
    rootMeta.node = globalThis.document.createTextNode("");
    rootMeta.parent = rootNodeMeta;
    rootNodeMeta.children.push(rootMeta);
    root.insertBefore(rootMeta.node, root.firstChild);
    return fragment2(toList2(children));
  }
  if (children.length === 1) {
    return children[0][1];
  }
  const placeholder = globalThis.document.createTextNode("");
  insertMetadataChild(text_kind, rootMeta, placeholder, 0, null);
  root.insertBefore(placeholder, root.firstChild);
  return none();
};
var virtualiseChild = (meta, domParent, child, index) => {
  if (child.nodeType === COMMENT_NODE) {
    const data = child.data.trim();
    if (data.startsWith("lustre:fragment")) {
      return virtualiseFragment(meta, domParent, child, index);
    }
    if (data.startsWith("lustre:map")) {
      return virtualiseMap(meta, domParent, child, index);
    }
    if (data.startsWith("lustre:memo")) {
      return virtualiseMemo(meta, domParent, child, index);
    }
    return null;
  }
  if (child.nodeType === ELEMENT_NODE) {
    return virtualiseElement(meta, child, index);
  }
  if (child.nodeType === TEXT_NODE) {
    return virtualiseText(meta, child, index);
  }
  return null;
};
var virtualiseElement = (metaParent, node, index) => {
  const key = node.getAttribute("data-lustre-key") ?? "";
  if (key) {
    node.removeAttribute("data-lustre-key");
  }
  const meta = insertMetadataChild(element_kind, metaParent, node, index, key);
  const tag = node.localName;
  const namespace = node.namespaceURI;
  const isHtmlElement = !namespace || namespace === NAMESPACE_HTML;
  if (isHtmlElement && INPUT_ELEMENTS.includes(tag)) {
    virtualiseInputEvents(tag, node);
  }
  const attributes = virtualiseAttributes(node);
  const { children } = virtualiseChildren(meta, node, node.firstChild);
  const vnode = isHtmlElement ? element3(tag, attributes, toList2(children)) : namespaced2(namespace, tag, attributes, toList2(children));
  return childResult(key, vnode, node.nextSibling);
};
var virtualiseChildren = (meta, domParent, childNode) => {
  const children = [];
  while (childNode && (childNode.nodeType !== COMMENT_NODE || childNode.data.trim() !== "/lustre:fragment")) {
    const child = virtualiseChild(meta, domParent, childNode, children.length);
    if (child) {
      children.push([child.key, child.vnode]);
      childNode = child.next;
    } else {
      childNode = childNode.nextSibling;
    }
  }
  return { children, end: childNode };
};
var virtualiseText = (meta, node, index) => {
  insertMetadataChild(text_kind, meta, node, index, null);
  return childResult("", text2(node.data), node.nextSibling);
};
var virtualiseFragment = (metaParent, domParent, node, index) => {
  const key = parseKey(node.data);
  const meta = insertMetadataChild(fragment_kind, metaParent, node, index, key);
  const { children, end } = virtualiseChildren(meta, domParent, node.nextSibling);
  meta.endNode = end;
  const vnode = fragment2(toList2(children));
  return childResult(key, vnode, end?.nextSibling);
};
var virtualiseMap = (metaParent, domParent, node, index) => {
  const key = parseKey(node.data);
  const meta = insertMetadataChild(map_kind, metaParent, node, index, key);
  const child = virtualiseNextChild(meta, domParent, node, 0);
  if (!child)
    return null;
  const vnode = map5(child.vnode, (x) => x);
  return childResult(key, vnode, child.next);
};
var virtualiseMemo = (meta, domParent, node, index) => {
  const key = parseKey(node.data);
  const child = virtualiseNextChild(meta, domParent, node, index);
  if (!child)
    return null;
  domParent.removeChild(node);
  const vnode = memo2(toList2([ref({})]), () => child.vnode);
  return childResult(key, vnode, child.next);
};
var virtualiseNextChild = (meta, domParent, node, index) => {
  while (true) {
    node = node.nextSibling;
    if (!node)
      return null;
    const child = virtualiseChild(meta, domParent, node, index);
    if (child)
      return child;
  }
};
var childResult = (key, vnode, next) => {
  return { key, vnode, next };
};
var virtualiseAttributes = (node) => {
  const attributes = [];
  for (let i = 0;i < node.attributes.length; i++) {
    const attr = node.attributes[i];
    if (attr.name !== "xmlns") {
      attributes.push(attribute2(attr.localName, attr.value));
    }
  }
  return toList2(attributes);
};
var INPUT_ELEMENTS = ["input", "select", "textarea"];
var virtualiseInputEvents = (tag, node) => {
  const value = node.value;
  const checked = node.checked;
  if (tag === "input" && node.type === "checkbox" && !checked)
    return;
  if (tag === "input" && node.type === "radio" && !checked)
    return;
  if (node.type !== "checkbox" && node.type !== "radio" && !value)
    return;
  queueMicrotask(() => {
    node.value = value;
    node.checked = checked;
    node.dispatchEvent(new Event("input", { bubbles: true }));
    node.dispatchEvent(new Event("change", { bubbles: true }));
    if (globalThis.document.activeElement !== node) {
      node.dispatchEvent(new Event("blur", { bubbles: true }));
    }
  });
};
var parseKey = (data) => {
  const keyMatch = data.match(/key="([^"]*)"/);
  if (!keyMatch)
    return "";
  return unescapeKey(keyMatch[1]);
};
var unescapeKey = (key) => {
  return key.replace(/&lt;/g, "<").replace(/&gt;/g, ">").replace(/&quot;/g, '"').replace(/&amp;/g, "&").replace(/&#39;/g, "'");
};
var toList2 = (arr) => arr.reduceRight((xs, x) => List$NonEmpty(x, xs), empty_list);

// build/dev/javascript/lustre/lustre/runtime/client/runtime.ffi.mjs
var is_browser = () => !!globalThis.document;
class Runtime {
  constructor(root, [model, effects], view, update, options) {
    this.root = root;
    this.#model = model;
    this.#view = view;
    this.#update = update;
    this.root.addEventListener("context-request", (event) => {
      if (!(event.context && event.callback))
        return;
      if (!this.#contexts.has(event.context))
        return;
      event.stopImmediatePropagation();
      const context = this.#contexts.get(event.context);
      if (event.subscribe) {
        const unsubscribe = () => {
          context.subscribers = context.subscribers.filter((subscriber) => subscriber !== event.callback);
        };
        context.subscribers.push([event.callback, unsubscribe]);
        event.callback(context.value, unsubscribe);
      } else {
        event.callback(context.value);
      }
    });
    const decodeEvent = (event, path, name) => decode2(this.#cache, path, name, event);
    const dispatch2 = (event, data) => {
      const [cache, result] = dispatch(this.#cache, data);
      this.#cache = cache;
      if (Result$isOk(result)) {
        const handler = Result$Ok$0(result);
        if (handler.stop_propagation)
          event.stopPropagation();
        if (handler.prevent_default)
          event.preventDefault();
        this.dispatch(handler.message, false);
      }
    };
    this.#reconciler = new Reconciler(this.root, decodeEvent, dispatch2, options);
    this.#vdom = virtualise(this.root);
    this.#cache = new$4();
    this.#handleEffects(effects);
    this.#render();
  }
  root = null;
  dispatch(message, shouldFlush = false) {
    if (this.#shouldQueue) {
      this.#queue.push(message);
    } else {
      const [model, effects] = this.#update(this.#model, message);
      this.#model = model;
      this.#scheduleRender(shouldFlush);
      this.#handleEffects(effects);
    }
  }
  emit(event, data) {
    const target = this.root.host ?? this.root;
    target.dispatchEvent(new LustreEvent(event, data));
  }
  provide(key, value) {
    if (!this.#contexts.has(key)) {
      this.#contexts.set(key, { value, subscribers: [] });
    } else {
      const context = this.#contexts.get(key);
      if (isEqual2(context.value, value)) {
        return;
      }
      context.value = value;
      for (let i = context.subscribers.length - 1;i >= 0; i--) {
        const [subscriber, unsubscribe] = context.subscribers[i];
        if (!subscriber) {
          context.subscribers.splice(i, 1);
          continue;
        }
        subscriber(value, unsubscribe);
      }
    }
  }
  subscribe(key, decoder) {
    if (!key)
      return;
    this.#contextSubscriptions.get(key)?.();
    const target = this.root.host ?? this.root;
    target.dispatchEvent(new ContextRequestEvent(key, (value, unsubscribe) => {
      const previousUnsubscribe = this.#contextSubscriptions.get(key);
      if (previousUnsubscribe !== unsubscribe) {
        previousUnsubscribe?.();
      }
      const decoded = run(value, decoder);
      this.#contextSubscriptions.set(key, unsubscribe);
      if (Result$isOk(decoded)) {
        this.dispatch(Result$Ok$0(decoded), true);
      }
    }, true));
  }
  unsubscribe(key) {
    const unsubscribe = this.#contextSubscriptions.get(key);
    if (unsubscribe) {
      unsubscribe();
      this.#contextSubscriptions.delete(key);
    }
  }
  unsubscribeAll() {
    for (const [_, unsubscribe] of this.#contextSubscriptions) {
      unsubscribe?.();
    }
    this.#contextSubscriptions.clear();
  }
  #model;
  #view;
  #update;
  #vdom;
  #cache;
  #reconciler;
  #contexts = new Map;
  #contextSubscriptions = new Map;
  #shouldQueue = false;
  #queue = [];
  #beforePaint = empty_list;
  #afterPaint = empty_list;
  #renderTimer = null;
  #actions = {
    dispatch: (message) => this.dispatch(message),
    emit: (event, data) => this.emit(event, data),
    select: () => {},
    root: () => this.root,
    provide: (key, value) => this.provide(key, value),
    subscribe: (key, decoder) => this.subscribe(key, decoder),
    unsubscribe: (key) => this.unsubscribe(key)
  };
  #scheduleRender(shouldFlush = false) {
    if (this.#renderTimer)
      return;
    if (shouldFlush) {
      this.#renderTimer = "sync";
      queueMicrotask(() => this.#render());
    } else {
      this.#renderTimer = window.requestAnimationFrame(() => this.#render());
    }
  }
  #handleEffects(effects) {
    this.#shouldQueue = true;
    let updateCalledDuringEffects = false;
    while (true) {
      iterate(effects.synchronous, (effect) => effect(this.#actions));
      this.#beforePaint = append4(this.#beforePaint, effects.before_paint);
      this.#afterPaint = append4(this.#afterPaint, effects.after_paint);
      if (!this.#queue.length)
        break;
      const message = this.#queue.shift();
      [this.#model, effects] = this.#update(this.#model, message);
      updateCalledDuringEffects = true;
    }
    this.#shouldQueue = false;
    return updateCalledDuringEffects;
  }
  #handleAsyncEffects(effects) {
    if (this.#handleEffects(effects)) {
      this.#scheduleRender(true);
    }
  }
  #render() {
    this.#renderTimer = null;
    const next = this.#view(this.#model);
    const { patch, cache } = diff(this.#cache, this.#vdom, next);
    this.#cache = cache;
    this.#vdom = next;
    this.#reconciler.push(patch, memos(cache));
    if (List$isNonEmpty(this.#beforePaint)) {
      const effects = makeEffect(this.#beforePaint);
      this.#beforePaint = empty_list;
      queueMicrotask(() => this.#handleAsyncEffects(effects));
    }
    if (List$isNonEmpty(this.#afterPaint)) {
      const effects = makeEffect(this.#afterPaint);
      this.#afterPaint = empty_list;
      window.requestAnimationFrame(() => this.#handleAsyncEffects(effects));
    }
  }
}
function makeEffect(synchronous) {
  return {
    synchronous,
    after_paint: empty_list,
    before_paint: empty_list
  };
}
var copiedStyleSheets = new WeakMap;
class ContextRequestEvent extends Event {
  constructor(context, callback, subscribe) {
    super("context-request", { bubbles: true, composed: true });
    this.context = context;
    this.callback = callback;
    this.subscribe = subscribe;
  }
}

class LustreEvent extends CustomEvent {
  isLustreEvent = true;
  constructor(name, detail) {
    super(name, { detail, bubbles: true, composed: true });
  }
}

// build/dev/javascript/lustre/lustre/runtime/client/spa.ffi.mjs
class Spa {
  #runtime;
  constructor(root, [init, effects], update, view) {
    this.#runtime = new Runtime(root, [init, effects], view, update);
  }
  send(message) {
    if (Message$isEffectDispatchedMessage(message)) {
      this.dispatch(message.message, false);
    } else if (Message$isEffectEmitEvent(message)) {
      this.emit(message.name, message.data);
    } else if (Message$isSystemRequestedShutdown(message)) {}
  }
  dispatch(message) {
    this.#runtime.dispatch(message);
  }
  emit(event, data) {
    this.#runtime.emit(event, data);
  }
}
var start = ({ init, update, view }, selector, flags) => {
  if (!is_browser())
    return Result$Error(Error$NotABrowser());
  const root = selector instanceof HTMLElement ? selector : globalThis.document.querySelector(selector);
  if (!root)
    return Result$Error(Error$ElementNotFound(selector));
  return Result$Ok(new Spa(root, init(flags), update, view));
};

// build/dev/javascript/lustre/lustre.mjs
class ElementNotFound extends CustomType {
  constructor(selector) {
    super();
    this.selector = selector;
  }
}
var Error$ElementNotFound = (selector) => new ElementNotFound(selector);
class NotABrowser extends CustomType {
}
var Error$NotABrowser$const = new NotABrowser;
var Error$NotABrowser = () => Error$NotABrowser$const;
function application(init, update, view) {
  return new App(Option$None$const, init, update, view, default_config);
}
function start4(app, selector, arguments$) {
  return guard(!is_browser(), new Error2(Error$NotABrowser$const), () => {
    return start(app, selector, arguments$);
  });
}

// build/dev/javascript/lustre/lustre/event.mjs
function on(name, handler) {
  return event(name, map3(handler, (message) => {
    return new Handler(false, false, message);
  }), empty_list, never, never, 0, 0);
}
function on_click(message) {
  return on("click", success(message));
}
// build/dev/javascript/plinth/document_ffi.mjs
function querySelector(query) {
  let found = document.querySelector(query);
  if (!found) {
    return Result$Error();
  }
  return Result$Ok(found);
}

// build/dev/javascript/plinth/element_ffi.mjs
function innerText(element) {
  return element.innerText;
}
function scrollIntoView(element) {
  element.scrollIntoView({ behavior: "smooth", block: "nearest" });
}

// build/dev/javascript/plinth/plinth/browser/element.mjs
class BeforeBegin extends CustomType {
}
var Position$BeforeBegin$const = new BeforeBegin;
class AfterBegin extends CustomType {
}
var Position$AfterBegin$const = new AfterBegin;
class BeforeEnd extends CustomType {
}
var Position$BeforeEnd$const = new BeforeEnd;
class AfterEnd extends CustomType {
}
var Position$AfterEnd$const = new AfterEnd;
// build/dev/javascript/tag_browser/tag_browser.mjs
var FILEPATH = "src/tag_browser.gleam";

class Model extends CustomType {
  constructor(posts_by_tag, selected_tag) {
    super();
    this.posts_by_tag = posts_by_tag;
    this.selected_tag = selected_tag;
  }
}

class ToggleTag extends CustomType {
  constructor(tag) {
    super();
    this.tag = tag;
  }
}

class ScrollToView extends CustomType {
}
var Msg$ScrollToView$const = new ScrollToView;
function view_tag(tag, posts, selected) {
  let _block;
  if (selected) {
    _block = prepend(class$("selected-tag-name"), List$Empty$const);
  } else {
    _block = List$Empty$const;
  }
  let classes = _block;
  return div(prepend(class$("tag-name"), classes), prepend(button(prepend(on_click(new ToggleTag(tag)), List$Empty$const), prepend(text2("#" + tag), prepend(text2(" (" + to_string(length(posts)) + ")"), List$Empty$const))), List$Empty$const));
}
function tag_grid_view(model) {
  return div(prepend(class$("tag-grid"), List$Empty$const), fold(model.posts_by_tag, List$Empty$const, (acc, k, v) => {
    return prepend(view_tag(k, v, isEqual(new Some(k), model.selected_tag)), acc);
  }));
}
function posts_for_tag_view(model, tag, posts) {
  let _block;
  let _pipe = posts;
  let _pipe$1 = reverse(_pipe);
  _block = index_map(_pipe$1, (p2, i) => {
    let _block$1;
    if (i === 0) {
      _block$1 = prepend(id("scroll-target"), List$Empty$const);
    } else {
      _block$1 = List$Empty$const;
    }
    let attrs = _block$1;
    return div(prepend(class$("post-preview"), attrs), prepend(a(prepend(href("/" + p2.slug), List$Empty$const), prepend(div(prepend(class$("post-preview-title"), List$Empty$const), prepend(text2(p2.title), List$Empty$const)), prepend(p(prepend(class$("post-preview-description"), List$Empty$const), prepend(text2(p2.description), List$Empty$const)), List$Empty$const))), List$Empty$const));
  });
  let posts$1 = _block;
  return div(List$Empty$const, prepend(tag_grid_view(model), prepend(h2(List$Empty$const, prepend(text2(tag), prepend(text2(" (" + to_string(length(posts$1)) + ")"), List$Empty$const))), prepend(div(prepend(class$("tag-browser-list-container"), List$Empty$const), prepend(div(prepend(class$("post-list"), List$Empty$const), posts$1), List$Empty$const)), List$Empty$const))));
}
function no_posts_view(model, tag) {
  return div(List$Empty$const, prepend(tag_grid_view(model), prepend(p(List$Empty$const, prepend(text2("No posts found for tag #" + tag), List$Empty$const)), List$Empty$const)));
}
function view(model) {
  let $ = model.selected_tag;
  if ($ instanceof Some) {
    let tag = $[0];
    let $1 = get(model.posts_by_tag, tag);
    if ($1 instanceof Ok) {
      let $2 = $1[0];
      if ($2 instanceof Empty) {
        return no_posts_view(model, tag);
      } else {
        let posts = $2;
        return posts_for_tag_view(model, tag, posts);
      }
    } else {
      return no_posts_view(model, tag);
    }
  } else {
    return tag_grid_view(model);
  }
}
function update2(model, msg) {
  if (msg instanceof ToggleTag) {
    let tag = msg.tag;
    let $ = model.selected_tag;
    if ($ instanceof Some) {
      let curr_tag = $[0];
      if (curr_tag === tag) {
        return [
          new Model(model.posts_by_tag, Option$None$const),
          none2()
        ];
      } else {
        let other_tag = $[0];
        return [
          new Model(model.posts_by_tag, new Some(tag)),
          after_paint((dispatch, _) => {
            return dispatch(Msg$ScrollToView$const);
          })
        ];
      }
    } else {
      return [
        new Model(model.posts_by_tag, new Some(tag)),
        after_paint((dispatch, _) => {
          return dispatch(Msg$ScrollToView$const);
        })
      ];
    }
  } else {
    let $ = querySelector("#scroll-target");
    if ($ instanceof Ok) {
      let el = $[0];
      scrollIntoView(el);
    }
    return [model, none2()];
  }
}
function init(posts_by_tag) {
  return [new Model(posts_by_tag, Option$None$const), none2()];
}
function post_decoder() {
  return field("title", string2, (title) => {
    return field("slug", string2, (slug) => {
      return field("url", string2, (url) => {
        return field("date", int2, (seconds) => {
          return field("description", string2, (description) => {
            return field("excerpt", string2, (excerpt) => {
              return field("language", optional(string2), (language) => {
                return field("featured_image", optional(string2), (featured_image) => {
                  let contents = List$Empty$const;
                  let extras = make();
                  return success(new Post(title, slug, url, from_unix_seconds(seconds), description, excerpt, language, featured_image, contents, extras));
                });
              });
            });
          });
        });
      });
    });
  });
}
function posts_by_tag_decoder() {
  return dict2(string2, list2(post_decoder()));
}
function main() {
  let $ = querySelector("#lustre-data");
  let el;
  if ($ instanceof Ok) {
    el = $[0];
  } else {
    throw makeError("let_assert", FILEPATH, "tag_browser", 20, "main", "Pattern match failed, no pattern matched the value.", { value: $, start: 480, end: 539, pattern_start: 491, pattern_end: 497 });
  }
  let txt = innerText(el);
  let $1 = parse(txt, posts_by_tag_decoder());
  let posts_by_tag;
  if ($1 instanceof Ok) {
    posts_by_tag = $1[0];
  } else {
    throw makeError("let_assert", FILEPATH, "tag_browser", 22, "main", "Pattern match failed, no pattern matched the value.", { value: $1, start: 584, end: 653, pattern_start: 595, pattern_end: 611 });
  }
  let app = application(init, update2, view);
  let $2 = start4(app, "#app", posts_by_tag);
  if (!($2 instanceof Ok)) {
    throw makeError("let_assert", FILEPATH, "tag_browser", 25, "main", "Pattern match failed, no pattern matched the value.", { value: $2, start: 708, end: 766, pattern_start: 719, pattern_end: 724 });
  }
  return;
}

// .lustre/build/tag_browser.mjs
main();
