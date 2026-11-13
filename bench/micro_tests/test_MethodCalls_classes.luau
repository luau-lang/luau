-- --bench-args: --fflags=DebugLuauUserDefinedClasses,DebugLuauUserDefinedClassesRuntime,LuauEmitCallFeedback
local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

-- Port of test_MethodCalls.lua to user-defined classes

open class Toggle
  public state

  function __init(self, start_state)
    self.state = start_state
  end

  function value(self)
    return self.state
  end

  function activate(self)
    self.state = not self.state
    return self
  end
end

class NthToggle extends Toggle
  public count_max
  public counter

  function __init(self, start_state, max_counter)
    Toggle.__init(self, start_state)
    self.count_max = max_counter
    self.counter = 0
  end

  function activate(self)
    self.counter = self.counter + 1
    if self.counter >= self.count_max then
      Toggle.activate(self)
      self.counter = 0
    end
    return self
  end
end

function test()
  local start = os.clock()
  local N = 30000

  local val = 1
  local toggle = Toggle(val)
  for i=1,N do
    val = toggle:activate():value()
    val = toggle:activate():value()
    val = toggle:activate():value()
    val = toggle:activate():value()
    val = toggle:activate():value()
    val = toggle:activate():value()
    val = toggle:activate():value()
    val = toggle:activate():value()
    val = toggle:activate():value()
    val = toggle:activate():value()
  end
  print(val and "true" or "false")

  val = 1
  local ntoggle = NthToggle(val, 3)
  for i=1,N do
    val = ntoggle:activate():value()
    val = ntoggle:activate():value()
    val = ntoggle:activate():value()
    val = ntoggle:activate():value()
    val = ntoggle:activate():value()
    val = ntoggle:activate():value()
    val = ntoggle:activate():value()
    val = ntoggle:activate():value()
    val = ntoggle:activate():value()
    val = ntoggle:activate():value()
  end
  print(val and "true" or "false")
  return os.clock() - start
end

bench.runCode(test, "MethodCalls classes")
