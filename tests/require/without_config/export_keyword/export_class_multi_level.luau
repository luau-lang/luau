export class LivingThing
    public alive: boolean

    function isAlive(self): boolean
        return self.alive
    end
end

export class Animal extends LivingThing
    public species: string

    function describe(self): string
        return `a {self.species}`
    end
end

export class Cat extends Animal
    public name: string

    function greet(self): string
        return `Hi, I'm {self.name}, {self:describe()}`
    end
end
