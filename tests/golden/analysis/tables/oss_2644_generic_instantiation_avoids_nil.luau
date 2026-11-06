const methods = table.freeze({
  getValue = function<T>(self: Value<T>): T
    return self.val
  end
})

type Value<T> = { val: T } & typeof(methods)

const function _test(v: Value<number>)
  const _raw = v:getValue()
end