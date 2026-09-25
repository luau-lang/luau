open class Parent
    public function __le(self, other)
        return true
    end
end

class Child extends Parent
    public function __le(self, other)
        return false
    end
end
