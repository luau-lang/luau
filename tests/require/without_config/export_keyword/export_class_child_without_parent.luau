class Animal
    public species: string

    function live(self): string
        return "I am alive"
    end
end

export class Cat extends Animal
    public breed: string

    function describe(self): string
        return `I am a {self.breed} {self.species}`
    end
end
