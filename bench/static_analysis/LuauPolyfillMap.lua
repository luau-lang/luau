--!strict
-- #region Array
-- Array related
local Array = {}
local Object = {}
local Map = {}

type Array<T> = { [number]: T }
type callbackFn<K, V> = (element: V, key: K, map: Map<K, V>) -> ()
type callbackFnWithThisArg<K, V> = (thisArg: Object, value: V, key: K, map: Map<K, V>) -> ()
type Map<K, V> = {
	size: number,
	-- method definitions
	set: (self: Map<K, V>, K, V) -> Map<K, V>,
	get: (self: Map<K, V>, K) -> V | nil,
	clear: (self: Map<K, V>) -> (),
	delete: (self: Map<K, V>, K) -> boolean,
	forEach: (self: Map<K, V>, callback: callbackFn<K, V> | callbackFnWithThisArg<K, V>, thisArg: Object?) -> (),
	has: (self: Map<K, V>, K) -> boolean,
	keys: (self: Map<K, V>) -> Array<K>,
	values: (self: Map<K, V>) -> Array<V>,
	entries: (self: Map<K, V>) -> Array<Tuple<K, V>>,
	ipairs: (self: Map<K, V>) -> any,
	[K]: V,
	_map: { [K]: V },
	_array: { [number]: K },
}
type mapFn<T, U> = (element: T, index: number) -> U
type mapFnWithThisArg<T, U> = (thisArg: any, element: T, index: number) -> U
type Object = { [string]: any }
type Table<T, V> = { [T]: V }
type Tuple<T, V> = Array<T | V>

local Set = {}

-- #region Array
function Array.isArray(value: any): boolean
	if typeof(value) ~= "table" then
		return false
	end
	if next(value) == nil then
		-- an empty table is an empty array
		return true
	end

	local length = #value

	if length == 0 then
		return false
	end

	local count = 0
	local sum = 0
	for key in pairs(value) do
		if typeof(key) ~= "number" then
			return false
		end
		if key % 1 ~= 0 or key < 1 then
			return false
		end
		count += 1
		sum += key
	end

	return sum == (count * (count + 1) / 2)
end

function Array.from<T, U>(
	value: string | Array<T> | Object,
	mapFn: (mapFn<T, U> | mapFnWithThisArg<T, U>)?,
	thisArg: Object?
): Array<U>
	if value == nil then
		error("cannot create array from a nil value")
	end
	local valueType = typeof(value)

	local array = {}

	if valueType == "table" and Array.isArray(value) then
		if mapFn then
			for i = 1, #(value :: Array<T>) do
				if thisArg ~= nil then
					array[i] = (mapFn :: mapFnWithThisArg<T, U>)(thisArg, (value :: Array<T>)[i], i)
				else
					array[i] = (mapFn :: mapFn<T, U>)((value :: Array<T>)[i], i)
				end
			end
		else
			for i = 1, #(value :: Array<T>) do
				array[i] = (value :: Array<any>)[i]
			end
		end
	elseif instanceOf(value, Set) then
		if mapFn then
			for i, v in (value :: any):ipairs() do
				if thisArg ~= nil then
					array[i] = (mapFn :: mapFnWithThisArg<T, U>)(thisArg, v, i)
				else
					array[i] = (mapFn :: mapFn<T, U>)(v, i)
				end
			end
		else
			for i, v in (value :: any):ipairs() do
				array[i] = v
			end
		end
	elseif instanceOf(value, Map) then
		if mapFn then
			for i, v in (value :: any):ipairs() do
				if thisArg ~= nil then
					array[i] = (mapFn :: mapFnWithThisArg<T, U>)(thisArg, v, i)
				else
					array[i] = (mapFn :: mapFn<T, U>)(v, i)
				end
			end
		else
			for i, v in (value :: any):ipairs() do
				array[i] = v
			end
		end
	elseif valueType == "string" then
		if mapFn then
			for i = 1, (value :: string):len() do
				if thisArg ~= nil then
					array[i] = (mapFn :: mapFnWithThisArg<T, U>)(thisArg, (value :: any):sub(i, i), i)
				else
					array[i] = (mapFn :: mapFn<T, U>)((value :: any):sub(i, i), i)
				end
			end
		else
			for i = 1, (value :: string):len() do
				array[i] = (value :: any):sub(i, i)
			end
		end
	end

	return array
end

type callbackFnArrayMap<T, U> = (element: T, index: number, array: Array<T>) -> U
type callbackFnWithThisArgArrayMap<T, U, V> = (thisArg: V, element: T, index: number, array: Array<T>) -> U

-- Implements Javascript's `Array.prototype.map` as defined below
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map
function Array.map<T, U, V>(
	t: Array<T>,
	callback: callbackFnArrayMap<T, U> | callbackFnWithThisArgArrayMap<T, U, V>,
	thisArg: V?
): Array<U>
	if typeof(t) ~= "table" then
		error(string.format("Array.map called on %s", typeof(t)))
	end
	if typeof(callback) ~= "function" then
		error("callback is not a function")
	end

	local len = #t
	local A = {}
	local k = 1

	while k <= len do
		local kValue = t[k]

		if kValue ~= nil then
			local mappedValue

			if thisArg ~= nil then
				mappedValue = (callback :: callbackFnWithThisArgArrayMap<T, U, V>)(thisArg, kValue, k, t)
			else
				mappedValue = (callback :: callbackFnArrayMap<T, U>)(kValue, k, t)
			end

			A[k] = mappedValue
		end
		k += 1
	end

	return A
end

type Function = (any, any, number, any) -> any

-- Implements Javascript's `Array.prototype.reduce` as defined below
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reduce
function Array.reduce<T>(array: Array<T>, callback: Function, initialValue: any?): any
	if typeof(array) ~= "table" then
		error(string.format("Array.reduce called on %s", typeof(array)))
	end
	if typeof(callback) ~= "function" then
		error("callback is not a function")
	end

	local length = #array

	local value
	local initial = 1

	if initialValue ~= nil then
		value = initialValue
	else
		initial = 2
		if length == 0 then
			error("reduce of empty array with no initial value")
		end
		value = array[1]
	end

	for i = initial, length do
		value = callback(value, array[i], i, array)
	end

	return value
end

type callbackFnArrayForEach<T> = (element: T, index: number, array: Array<T>) -> ()
type callbackFnWithThisArgArrayForEach<T, U> = (thisArg: U, element: T, index: number, array: Array<T>) -> ()

-- Implements Javascript's `Array.prototype.forEach` as defined below
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/forEach
function Array.forEach<T, U>(
	t: Array<T>,
	callback: callbackFnArrayForEach<T> | callbackFnWithThisArgArrayForEach<T, U>,
	thisArg: U?
): ()
	if typeof(t) ~= "table" then
		error(string.format("Array.forEach called on %s", typeof(t)))
	end
	if typeof(callback) ~= "function" then
		error("callback is not a function")
	end

	local len = #t
	local k = 1

	while k <= len do
		local kValue = t[k]

		if thisArg ~= nil then
			(callback :: callbackFnWithThisArgArrayForEach<T, U>)(thisArg, kValue, k, t)
		else
			(callback :: callbackFnArrayForEach<T>)(kValue, k, t)
		end

		if #t < len then
			-- don't iterate on removed items, don't iterate more than original length
			len = #t
		end
		k += 1
	end
end
-- #endregion

-- #region Set
Set.__index = Set

type callbackFnSet<T> = (value: T, key: T, set: Set<T>) -> ()
type callbackFnWithThisArgSet<T> = (thisArg: Object, value: T, key: T, set: Set<T>) -> ()

export type Set<T> = {
	size: number,
	-- method definitions
	add: (self: Set<T>, T) -> Set<T>,
	clear: (self: Set<T>) -> (),
	delete: (self: Set<T>, T) -> boolean,
	forEach: (self: Set<T>, callback: callbackFnSet<T> | callbackFnWithThisArgSet<T>, thisArg: Object?) -> (),
	has: (self: Set<T>, T) -> boolean,
	ipairs: (self: Set<T>) -> any,
}

type Iterable = { ipairs: (any) -> any }

function Set.new<T>(iterable: Array<T> | Set<T> | Iterable | string | nil): Set<T>
	local array = {}
	local map = {}
	if iterable ~= nil then
		local arrayIterable: Array<any>
		-- ROBLOX TODO: remove type casting from (iterable :: any).ipairs in next release
		if typeof(iterable) == "table" then
			if Array.isArray(iterable) then
				arrayIterable = Array.from(iterable :: Array<any>)
			elseif typeof((iterable :: Iterable).ipairs) == "function" then
				-- handle in loop below
			elseif _G.__DEV__ then
				error("cannot create array from an object-like table")
			end
		elseif typeof(iterable) == "string" then
			arrayIterable = Array.from(iterable :: string)
		else
			error(("cannot create array from value of type `%s`"):format(typeof(iterable)))
		end

		if arrayIterable then
			for _, element in ipairs(arrayIterable) do
				if not map[element] then
					map[element] = true
					table.insert(array, element)
				end
			end
		elseif typeof(iterable) == "table" and typeof((iterable :: Iterable).ipairs) == "function" then
			for _, element in (iterable :: Iterable):ipairs() do
				if not map[element] then
					map[element] = true
					table.insert(array, element)
				end
			end
		end
	end

	return (setmetatable({
		size = #array,
		_map = map,
		_array = array,
	}, Set) :: any) :: Set<T>
end

function Set:add(value)
	if not self._map[value] then
		-- Luau FIXME: analyze should know self is Set<T> which includes size as a number
		self.size = self.size :: number + 1
		self._map[value] = true
		table.insert(self._array, value)
	end
	return self
end

function Set:clear()
	self.size = 0
	table.clear(self._map)
	table.clear(self._array)
end

function Set:delete(value): boolean
	if not self._map[value] then
		return false
	end
	-- Luau FIXME: analyze should know self is Map<K, V> which includes size as a number
	self.size = self.size :: number - 1
	self._map[value] = nil
	local index = table.find(self._array, value)
	if index then
		table.remove(self._array, index)
	end
	return true
end

-- Implements Javascript's `Map.prototype.forEach` as defined below
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/forEach
function Set:forEach<T>(callback: callbackFnSet<T> | callbackFnWithThisArgSet<T>, thisArg: Object?): ()
	if typeof(callback) ~= "function" then
		error("callback is not a function")
	end

	return Array.forEach(self._array, function(value: T)
		if thisArg ~= nil then
			(callback :: callbackFnWithThisArgSet<T>)(thisArg, value, value, self)
		else
			(callback :: callbackFnSet<T>)(value, value, self)
		end
	end)
end

function Set:has(value): boolean
	return self._map[value] ~= nil
end

function Set:ipairs()
	return ipairs(self._array)
end

-- #endregion Set

-- #region Object
function Object.entries(value: string | Object | Array<any>): Array<any>
	assert(value :: any ~= nil, "cannot get entries from a nil value")
	local valueType = typeof(value)

	local entries: Array<Tuple<string, any>> = {}
	if valueType == "table" then
		for key, keyValue in pairs(value :: Object) do
			-- Luau FIXME: Luau should see entries as Array<any>, given object is [string]: any, but it sees it as Array<Array<string>> despite all the manual annotation
			table.insert(entries, { key :: string, keyValue :: any })
		end
	elseif valueType == "string" then
		for i = 1, string.len(value :: string) do
			entries[i] = { tostring(i), string.sub(value :: string, i, i) }
		end
	end

	return entries
end

-- #endregion

-- #region instanceOf

-- ROBLOX note: Typed tbl as any to work with strict type analyze
-- polyfill for https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof
function instanceOf(tbl: any, class)
	assert(typeof(class) == "table", "Received a non-table as the second argument for instanceof")

	if typeof(tbl) ~= "table" then
		return false
	end

	local ok, hasNew = pcall(function()
		return class.new ~= nil and tbl.new == class.new
	end)
	if ok and hasNew then
		return true
	end

	local seen = { tbl = true }

	while tbl and typeof(tbl) == "table" do
		tbl = getmetatable(tbl)
		if typeof(tbl) == "table" then
			tbl = tbl.__index

			if tbl == class then
				return true
			end
		end

		-- if we still have a valid table then check against seen
		if typeof(tbl) == "table" then
			if seen[tbl] then
				return false
			end
			seen[tbl] = true
		end
	end

	return false
end
-- #endregion

function Map.new<K, V>(iterable: Array<Array<any>>?): Map<K, V>
	local array = {}
	local map = {}
	if iterable ~= nil then
		local arrayFromIterable
		local iterableType = typeof(iterable)
		if iterableType == "table" then
			if #iterable > 0 and typeof(iterable[1]) ~= "table" then
				error("cannot create Map from {K, V} form, it must be { {K, V}... }")
			end

			arrayFromIterable = Array.from(iterable)
		else
			error(("cannot create array from value of type `%s`"):format(iterableType))
		end

		for _, entry in ipairs(arrayFromIterable) do
			local key = entry[1]
			if _G.__DEV__ then
				if key == nil then
					error("cannot create Map from a table that isn't an array.")
				end
			end
			local val = entry[2]
			-- only add to array if new
			if map[key] == nil then
				table.insert(array, key)
			end
			-- always assign
			map[key] = val
		end
	end

	return (setmetatable({
		size = #array,
		_map = map,
		_array = array,
	}, Map) :: any) :: Map<K, V>
end

function Map:set<K, V>(key: K, value: V): Map<K, V>
	-- preserve initial insertion order
	if self._map[key] == nil then
		-- Luau FIXME: analyze should know self is Map<K, V> which includes size as a number
		self.size = self.size :: number + 1
		table.insert(self._array, key)
	end
	-- always update value
	self._map[key] = value
	return self
end

function Map:get(key)
	return self._map[key]
end

function Map:clear()
	local table_: any = table
	self.size = 0
	table_.clear(self._map)
	table_.clear(self._array)
end

function Map:delete(key): boolean
	if self._map[key] == nil then
		return false
	end
	-- Luau FIXME: analyze should know self is Map<K, V> which includes size as a number
	self.size = self.size :: number - 1
	self._map[key] = nil
	local index = table.find(self._array, key)
	if index then
		table.remove(self._array, index)
	end
	return true
end

-- Implements Javascript's `Map.prototype.forEach` as defined below
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map/forEach
function Map:forEach<K, V>(callback: callbackFn<K, V> | callbackFnWithThisArg<K, V>, thisArg: Object?): ()
	if typeof(callback) ~= "function" then
		error("callback is not a function")
	end

	return Array.forEach(self._array, function(key: K)
		local value: V = self._map[key] :: V

		if thisArg ~= nil then
			(callback :: callbackFnWithThisArg<K, V>)(thisArg, value, key, self)
		else
			(callback :: callbackFn<K, V>)(value, key, self)
		end
	end)
end

function Map:has(key): boolean
	return self._map[key] ~= nil
end

function Map:keys()
	return self._array
end

function Map:values()
	return Array.map(self._array, function(key)
		return self._map[key]
	end)
end

function Map:entries()
	return Array.map(self._array, function(key)
		return { key, self._map[key] }
	end)
end

function Map:ipairs()
	return ipairs(self:entries())
end

function Map.__index(self, key)
	local mapProp = rawget(Map, key)
	if mapProp ~= nil then
		return mapProp
	end

	return Map.get(self, key)
end

function Map.__newindex(table_, key, value)
	table_:set(key, value)
end

local function coerceToMap(mapLike: Map<any, any> | Table<any, any>): Map<any, any>
	return instanceOf(mapLike, Map) and mapLike :: Map<any, any> -- ROBLOX: order is preservered
		or Map.new(Object.entries(mapLike)) -- ROBLOX: order is not preserved
end

local function coerceToTable(mapLike: Map<any, any> | Table<any, any>): Table<any, any>
	if not instanceOf(mapLike, Map) then
		return mapLike
	end

	-- create table from map
	return Array.reduce(mapLike:entries(), function(tbl, entry)
		tbl[entry[1]] = entry[2]
		return tbl
	end, {})
end

return {
	Map = Map,
	coerceToMap = coerceToMap,
	coerceToTable = coerceToTable,
}
