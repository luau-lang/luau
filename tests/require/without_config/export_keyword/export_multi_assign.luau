export local a = 0
export local b = 0

export function update()
	a, b = 1, 2
end

export function readA()
	return a
end

export function readB()
	return b
end
