// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/RequireTracer.h"

#include "Fixture.h"

#include "doctest.h"

#include <algorithm>

using namespace Luau;

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)
LUAU_FASTFLAG(DebugLuauFreezeArena);
LUAU_FASTFLAG(DebugLuauMagicTypes);

LUAU_FASTFLAG(StudioReportLuauAny);

struct ATSFixture: BuiltinsFixture
{
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    ATSFixture()
    {
        addGlobalBinding(frontend.globals, "game", builtinTypes->anyType, "@test");
        addGlobalBinding(frontend.globals, "script", builtinTypes->anyType, "@test");
    }
};

TEST_SUITE_BEGIN("AnyTypeSummaryTest");

TEST_CASE_FIXTURE(ATSFixture, "var_typepack_any")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
type A = (number, string) -> ...any
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 1);
        LUAU_ASSERT((int)module->ats.typeInfo[0].code == 7); // Alias
    }
}

TEST_CASE_FIXTURE(ATSFixture, "export_alias")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
export type t8<t8> =  t0 &(<t0 ...>(true | any)->(''))
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_ERROR_COUNT(1, result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
    }
}

TEST_CASE_FIXTURE(ATSFixture, "var_typepack_any_gen_table")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
type Pair<T> = {first: T, second: any}
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 1);
        LUAU_ASSERT((int)module->ats.typeInfo[0].code == 7); // Alias
    }
}

TEST_CASE_FIXTURE(ATSFixture, "assign_uneq")
{
    fileResolver.source["game/Gui/Modules/B"] = R"(
local function greetings(name: string)
    return "Hello, " .. name, nil
end

local x, y = greetings("Dibri")
local x, y = greetings("Dibri"), nil
local x, y, z = greetings("Dibri") -- mismatch
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/B");
    LUAU_REQUIRE_ERROR_COUNT(1, result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/B");
    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 0);
    }
}

TEST_CASE_FIXTURE(ATSFixture, "var_typepack_any_gen")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
-- type Pair<T> = (boolean, string, ...any) -> {T} -- type aliases with generics/pack do not seem to be processed?
type Pair<T> = (boolean, T) -> ...any
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 1);
        LUAU_ASSERT((int)module->ats.typeInfo[0].code == 7); // Alias
    }
}

TEST_CASE_FIXTURE(ATSFixture, "generic_types")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
local function foo<A>(a: (...A) -> any, ...: A)
	return a(...)
end

local function addNumbers(num1, num2)
	local result = num1 + num2
	return result
end

foo(addNumbers)
    )";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");
    
    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 3);
        LUAU_ASSERT((int)module->ats.typeInfo[1].code == 3); // FuncApp
    }
}

TEST_CASE_FIXTURE(ATSFixture, "no_annot")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
local character = script.Parent
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 0);
    }
}

TEST_CASE_FIXTURE(ATSFixture, "if_any")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
function f(x: any)
if not x then
x = {
    y = math.random(0, 2^31-1),
    left = nil,
    right = nil
}
else
    local expected = x * 5
end
end
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 1);
        LUAU_ASSERT((int)module->ats.typeInfo[0].code == 1); // Func Arg
    }
}

TEST_CASE_FIXTURE(ATSFixture, "variadic_any")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
    local function f(): (number, ...any) -- double count "any" ret if varags
    return 1, 5
    end

    local x, y, z = f() -- not catching this any because no annot
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 1);       // do we need this to be the var arg pattern?
        LUAU_ASSERT((int)module->ats.typeInfo[0].code == 2); // Func Ret
    }
}

TEST_CASE_FIXTURE(ATSFixture, "type_alias_intersection")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
    type XCoord = {x: number}
    type YCoord = {y: any}
    type Vector2 = XCoord & YCoord -- table type intersections do not get normalized
    local vec2: Vector2 = {x = 1, y = 2}
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 3);
        LUAU_ASSERT((int)module->ats.typeInfo[2].code == 4); // Variable Annotation
    }
}

TEST_CASE_FIXTURE(ATSFixture, "var_func_arg")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
    local function f(...: any)
    end
    local function f(x: number?, y, z: any)
    end
    function f(x: number?, y, z: any)
    end
    function f(...: any)
    end
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 4);
        LUAU_ASSERT((int)module->ats.typeInfo[0].code == 5); // Variadic Any
    }
}

TEST_CASE_FIXTURE(ATSFixture, "var_func_apps")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
    local function f(...: any)
    end
    f("string", 123)
    f("string")
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 3);
        LUAU_ASSERT((int)module->ats.typeInfo[0].code == 5); // Variadic Any
    }
}


TEST_CASE_FIXTURE(ATSFixture, "CannotExtendTable")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
local CAR_COLLISION_GROUP = "Car"

-- Set the car collision group
for _, descendant in carTemplate:GetDescendants() do
    if descendant:IsA("BasePart") then
        descendant.CollisionGroup = CAR_COLLISION_GROUP
    end
end

)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_ERROR_COUNT(3, result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 0);
    }
}

TEST_CASE_FIXTURE(ATSFixture, "unknown_symbol")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
local function manageRace(raceContainer: Model)
	RaceManager.new(raceContainer)
end

)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_ERROR_COUNT(2, result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");
    
    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 2);
    }
}

TEST_CASE_FIXTURE(ATSFixture, "racing_3_short")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(

local CollectionService = game:GetService("CollectionService")

local RaceManager = require(script.RaceManager)

local RACE_TAG = "Race"

local function manageRace(raceContainer: Model)
	RaceManager.new(raceContainer)
end

local function initialize()
	CollectionService:GetInstanceAddedSignal(RACE_TAG):Connect(manageRace)

	for _, raceContainer in CollectionService:GetTagged(RACE_TAG) do
		manageRace(raceContainer)
	end
end

initialize()

)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_ERROR_COUNT(2, result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 5); //unknown Model -- why are we inferring so many anys here --prbably something with the data model. worth looking into
    }
}

TEST_CASE_FIXTURE(ATSFixture, "racing_collision_2")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
local PhysicsService = game:GetService("PhysicsService")
local ReplicatedStorage = game:GetService("ReplicatedStorage")

local safePlayerAdded = require(script.safePlayerAdded)

local CAR_COLLISION_GROUP = "Car"
local CHARACTER_COLLISION_GROUP = "Character"

local carTemplate = ReplicatedStorage.Car

local function onCharacterAdded(character: Model)
	-- Set the collision group for any parts that are added to the character
	character.DescendantAdded:Connect(function(descendant)
		if descendant:IsA("BasePart") then
			descendant.CollisionGroup = CHARACTER_COLLISION_GROUP
		end
	end)

	-- Set the collision group for any parts currently in the character
	for _, descendant in character:GetDescendants() do
		if descendant:IsA("BasePart") then
			descendant.CollisionGroup = CHARACTER_COLLISION_GROUP
		end
	end
end

local function onPlayerAdded(player: Player)
	player.CharacterAdded:Connect(onCharacterAdded)

	if player.Character then
		onCharacterAdded(player.Character)
	end
end

local function initialize()
	-- Setup collision groups
	PhysicsService:RegisterCollisionGroup(CAR_COLLISION_GROUP)
	PhysicsService:RegisterCollisionGroup(CHARACTER_COLLISION_GROUP)

	-- Stop the collision groups from colliding with each other
	PhysicsService:CollisionGroupSetCollidable(CAR_COLLISION_GROUP, CAR_COLLISION_GROUP, false)
	PhysicsService:CollisionGroupSetCollidable(CHARACTER_COLLISION_GROUP, CHARACTER_COLLISION_GROUP, false)
	PhysicsService:CollisionGroupSetCollidable(CAR_COLLISION_GROUP, CHARACTER_COLLISION_GROUP, false)

	-- Set the car collision group
	for _, descendant in carTemplate:GetDescendants() do
		if descendant:IsA("BasePart") then
			descendant.CollisionGroup = CAR_COLLISION_GROUP
		end
	end

	-- Set character collision groups for all players
	safePlayerAdded(onPlayerAdded)
end

initialize()

)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_ERROR_COUNT(5, result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 11);
        LUAU_ASSERT((int)module->ats.typeInfo[0].code == 1); // Func Arg
    }
}

TEST_CASE_FIXTURE(ATSFixture, "racing_spawning_1")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
local CollectionService = game:GetService("CollectionService")
local Players = game:GetService("Players")

local spawnCar = require(script.spawnCar)
local destroyPlayerCars = require(script.destroyPlayerCars)

local spawnPromptTemplate = script.SpawnPrompt

local KIOSK_TAG = "CarSpawnKiosk"

local function setupKiosk(kiosk: Model)
	local spawnLocation = kiosk:FindFirstChild("SpawnLocation")
	assert(spawnLocation, `{kiosk:GetFullName()} has no SpawnLocation part`)
	local promptPart = kiosk:FindFirstChild("Prompt")
	assert(promptPart, `{kiosk:GetFullName()} has no Prompt part`)

	-- Hide the car spawn location
	spawnLocation.Transparency = 1

	-- Create a new prompt to spawn the car
	local spawnPrompt = spawnPromptTemplate:Clone()
	spawnPrompt.Parent = promptPart

	spawnPrompt.Triggered:Connect(function(player: Player)
		-- Remove any existing cars the player has spawned
		destroyPlayerCars(player)
		-- Spawn a new car at the spawnLocation, owned by the player
		spawnCar(spawnLocation.CFrame, player)
	end)
end

local function initialize()
	-- Remove cars owned by players whenever they leave
	Players.PlayerRemoving:Connect(destroyPlayerCars)

	-- Setup all car spawning kiosks
	CollectionService:GetInstanceAddedSignal(KIOSK_TAG):Connect(setupKiosk)

	for _, kiosk in CollectionService:GetTagged(KIOSK_TAG) do
		setupKiosk(kiosk)
	end
end

initialize()

)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_ERROR_COUNT(5, result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 7);
        LUAU_ASSERT((int)module->ats.typeInfo[0].code == 1); // Func Arg
    }
}

TEST_CASE_FIXTURE(ATSFixture, "mutually_recursive_generic")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
        --!strict
        type T<a> = { f: a, g: U<a> }
        type U<a> = { h: a, i: T<a>? }
        local x: T<number> = { f = 37, g = { h = 5, i = nil } }
        x.g.i = x
        local y: T<string> = { f = "hi", g = { h = "lo", i = nil } }
        y.g.i = y
    )";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_ERROR_COUNT(2, result1);
}

TEST_CASE_FIXTURE(ATSFixture, "explicit_pack")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
type Foo<T...> = (T...) -> () -- also want to see how these are used.
type Bar = Foo<(number, any)>
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 1);
        LUAU_ASSERT((int)module->ats.typeInfo[0].code == 7); // Alias
    }
}

TEST_CASE_FIXTURE(ATSFixture, "var_any_local")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
local x = 2
local x: any = 2, 3
local x: any, y = 1, 2
local x: number, y: any, z, h: nil = 1, nil
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 3);
        LUAU_ASSERT((int)module->ats.typeInfo[0].code == 4); // Variable Annotation
    }
}

TEST_CASE_FIXTURE(ATSFixture, "table_uses_any")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
    local x: any = 0
    local y: number
    local z = {x=x, y=y} -- not catching this
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 1);
        LUAU_ASSERT((int)module->ats.typeInfo[0].code == 4); // Variable Annotation
    }
}

TEST_CASE_FIXTURE(ATSFixture, "typeof_any")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
    local x: any = 0
    function some1(x: typeof(x))
    end
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 2);
        LUAU_ASSERT((int)module->ats.typeInfo[1].code == 1); // Function Arguments
    }
}

TEST_CASE_FIXTURE(ATSFixture, "table_type_assigned")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
    local x: { x:  any?} = {x = 1}
    local z: { x : any, y : number? } -- not catching this
    z.x = "bigfatlongstring"
    z.y = nil
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 2);       // double counting variable annot again
        LUAU_ASSERT((int)module->ats.typeInfo[1].code == 8); // Assign
    }
}

TEST_CASE_FIXTURE(ATSFixture, "simple_func_wo_ret")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
    function some(x: any)
    end
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 1);
        LUAU_ASSERT((int)module->ats.typeInfo[0].code == 1); // Function Arguments
    }
}

TEST_CASE_FIXTURE(ATSFixture, "simple_func_w_ret")
{
    // TODO: should only return 1
    fileResolver.source["game/Gui/Modules/A"] = R"(
    function other(y: number): any
    return "gotcha!"
    end
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 1);
        LUAU_ASSERT((int)module->ats.typeInfo[0].code == 2); // Function Return
    }
}

TEST_CASE_FIXTURE(ATSFixture, "nested_local")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
    function cool(y: number): number
    local g: any = "gratatataaa"
    return y
    end
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 1);       // should be one, double counitng annot
        LUAU_ASSERT((int)module->ats.typeInfo[0].code == 4); // Variable Annotation
    }
}

TEST_CASE_FIXTURE(ATSFixture, "generic_func")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
    function reverse<T>(a: {T}, b: any): {T}
    return a
    end
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 1);
        LUAU_ASSERT((int)module->ats.typeInfo[0].code == 1); // Function Arguments
    }
}

TEST_CASE_FIXTURE(ATSFixture, "type_alias_any")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
    type Clear = any
    local z: Clear = "zip"  
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    if (FFlag::StudioReportLuauAny)
    {
        LUAU_ASSERT(module->ats.typeInfo.size() == 2);
        LUAU_ASSERT((int)module->ats.typeInfo[0].code == 7); // Alias
    }
}

TEST_CASE_FIXTURE(ATSFixture, "multi_module_any")
{
    fileResolver.source["game/A"] = R"(
    export type MyFunction = (number, string) -> (any)
)";

    fileResolver.source["game/B"] = R"(
    local MyFunc = require(script.Parent.A)
    type Clear = any
    local z: Clear = "zip"  
)";

    fileResolver.source["game/Gui/Modules/A"] = R"(
    local Modules = game:GetService('Gui').Modules
    local B = require(Modules.B)
    return {hello = B.hello}
)";

    CheckResult result = frontend.check("game/B");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_SUITE_END();
