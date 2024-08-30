// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/RequireTracer.h"

#include "Fixture.h"

#include "ScopedFlags.h"
#include "doctest.h"

#include <algorithm>

using namespace Luau;

using Pattern = AnyTypeSummary::Pattern;

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(DebugLuauFreezeArena)
LUAU_FASTFLAG(DebugLuauMagicTypes)
LUAU_FASTFLAG(StudioReportLuauAny2)


struct ATSFixture : BuiltinsFixture
{

    ATSFixture()
    {
        addGlobalBinding(frontend.globals, "game", builtinTypes->anyType, "@test");
        addGlobalBinding(frontend.globals, "script", builtinTypes->anyType, "@test");
    }
};

TEST_SUITE_BEGIN("AnyTypeSummaryTest");

TEST_CASE_FIXTURE(ATSFixture, "var_typepack_any")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
type A = (number, string) -> ...any
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 1);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::Alias);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "type A = (number, string)->( ...any)");
}

TEST_CASE_FIXTURE(ATSFixture, "export_alias")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
export type t8<t8> =  t0 &(<t0 ...>(true | any)->(''))
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_ERROR_COUNT(1, result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 1);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::Alias);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "export type t8<t8> =  t0 &(<t0 ...>(true | any)->(''))");
}

TEST_CASE_FIXTURE(ATSFixture, "typepacks")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
local function fallible(t: number): ...any
	if t > 0 then
		return true, t -- should catch this
	end
	return false, "must be positive" -- should catch this
end
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 3);
    LUAU_ASSERT(module->ats.typeInfo[1].code == Pattern::TypePk);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "local function fallible(t: number): ...any\n if t > 0 then\n  return true, t\n end\n return false, 'must be positive'\nend");
}

TEST_CASE_FIXTURE(ATSFixture, "typepacks_no_ret")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
-- TODO: if partially typed, we'd want to know too
local function fallible(t: number)
	if t > 0 then
		return true, t 
	end
	return false, "must be positive"
end
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_ERROR_COUNT(1, result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 0);
}

TEST_CASE_FIXTURE(ATSFixture, "var_typepack_any_gen_table")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
type Pair<T> = {first: T, second: any}
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 1);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::Alias);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "type Pair<T> = {first: T, second: any}");
}

TEST_CASE_FIXTURE(ATSFixture, "assign_uneq")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

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
    LUAU_ASSERT(module->ats.typeInfo.size() == 0);
}

TEST_CASE_FIXTURE(ATSFixture, "var_typepack_any_gen")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
-- type Pair<T> = (boolean, string, ...any) -> {T} -- type aliases with generics/pack do not seem to be processed?
type Pair<T> = (boolean, T) -> ...any
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 1);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::Alias);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "type Pair<T> = (boolean, T)->( ...any)");
}

TEST_CASE_FIXTURE(ATSFixture, "typeof_any_in_func")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
    local function f()
        local a: any = 1
        local b: typeof(a) = 1
    end
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 2);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::VarAnnot);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "local function f()\n        local a: any = 1\n        local b: typeof(a) = 1\n    end");
}

TEST_CASE_FIXTURE(ATSFixture, "generic_types")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

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

    LUAU_ASSERT(module->ats.typeInfo.size() == 3);
    LUAU_ASSERT(module->ats.typeInfo[1].code == Pattern::FuncApp);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "local function foo<A>(a: (...A)->( any),...: A)\n return a(...)\nend");
}

TEST_CASE_FIXTURE(ATSFixture, "no_annot")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
local character = script.Parent
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 0);
}

TEST_CASE_FIXTURE(ATSFixture, "if_any")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

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

    LUAU_ASSERT(module->ats.typeInfo.size() == 1);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::FuncArg);
    LUAU_ASSERT(
        module->ats.typeInfo[0].node == "function f(x: any)\nif not x then\nx = {\n    y = math.random(0, 2^31-1),\n    left = nil,\n    right = "
                                        "nil\n}\nelse\n    local expected = x * 5\nend\nend"
    );
}

TEST_CASE_FIXTURE(ATSFixture, "variadic_any")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
    local function f(): (number, ...any)
    return 1, 5 --catching this
    end

    local x, y, z = f() -- not catching this any because no annot
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 2);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::FuncRet);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "local function f(): (number, ...any)\n    return 1, 5\n    end");
}

TEST_CASE_FIXTURE(ATSFixture, "type_alias_intersection")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
    type XCoord = {x: number}
    type YCoord = {y: any}
    type Vector2 = XCoord & YCoord -- table type intersections do not get normalized
    local vec2: Vector2 = {x = 1, y = 2}
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 3);
    LUAU_ASSERT(module->ats.typeInfo[2].code == Pattern::VarAnnot);
    LUAU_ASSERT(module->ats.typeInfo[2].node == "local vec2: Vector2 = {x = 1, y = 2}");
}

TEST_CASE_FIXTURE(ATSFixture, "var_func_arg")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

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

    LUAU_ASSERT(module->ats.typeInfo.size() == 4);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::VarAny);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "local function f(...: any)\n    end");
}

TEST_CASE_FIXTURE(ATSFixture, "var_func_apps")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
    local function f(...: any)
    end
    f("string", 123)
    f("string")
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 3);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::VarAny);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "local function f(...: any)\n    end");
}


TEST_CASE_FIXTURE(ATSFixture, "CannotExtendTable")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

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

    LUAU_ASSERT(module->ats.typeInfo.size() == 0);
}

TEST_CASE_FIXTURE(ATSFixture, "unknown_symbol")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
local function manageRace(raceContainer: Model)
	RaceManager.new(raceContainer)
end

)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_ERROR_COUNT(2, result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 2);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::FuncArg);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "local function manageRace(raceContainer: Model)\n RaceManager.new(raceContainer)\nend");
}

TEST_CASE_FIXTURE(ATSFixture, "racing_3_short")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

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

    LUAU_ASSERT(module->ats.typeInfo.size() == 5);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::FuncArg);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "local function manageRace(raceContainer: Model)\n RaceManager.new(raceContainer)\nend");
}

TEST_CASE_FIXTURE(ATSFixture, "racing_collision_2")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

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

    LUAU_ASSERT(module->ats.typeInfo.size() == 11);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::FuncArg);
    LUAU_ASSERT(
        module->ats.typeInfo[0].node ==
        "local function onCharacterAdded(character: Model)\n\n character.DescendantAdded:Connect(function(descendant)\n  if "
        "descendant:IsA('BasePart')then\n   descendant.CollisionGroup = CHARACTER_COLLISION_GROUP\n  end\n end)\n\n\n for _, descendant in "
        "character:GetDescendants()do\n  if descendant:IsA('BasePart')then\n   descendant.CollisionGroup = CHARACTER_COLLISION_GROUP\n  end\n "
        "end\nend"
    );
}

TEST_CASE_FIXTURE(ATSFixture, "racing_spawning_1")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

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

    LUAU_ASSERT(module->ats.typeInfo.size() == 7);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::FuncArg);
    LUAU_ASSERT(
        module->ats.typeInfo[0].node ==
        "local function setupKiosk(kiosk: Model)\n local spawnLocation = kiosk:FindFirstChild('SpawnLocation')\n assert(spawnLocation, "
        "`{kiosk:GetFullName()} has no SpawnLocation part`)\n local promptPart = kiosk:FindFirstChild('Prompt')\n assert(promptPart, "
        "`{kiosk:GetFullName()} has no Prompt part`)\n\n\n spawnLocation.Transparency = 1\n\n\n local spawnPrompt = "
        "spawnPromptTemplate:Clone()\n spawnPrompt.Parent = promptPart\n\n spawnPrompt.Triggered:Connect(function(player: Player)\n\n  "
        "destroyPlayerCars(player)\n\n  spawnCar(spawnLocation.CFrame, player)\n end)\nend"
    );
}

TEST_CASE_FIXTURE(ATSFixture, "mutually_recursive_generic")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

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

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 0);
}

TEST_CASE_FIXTURE(ATSFixture, "explicit_pack")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
type Foo<T...> = (T...) -> () -- also want to see how these are used.
type Bar = Foo<(number, any)>
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 1);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::Alias);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "type Bar = Foo<(number, any)>");
}

TEST_CASE_FIXTURE(ATSFixture, "local_val")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
local a, b, c = 1 :: any
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 1);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::Casts);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "local a, b, c = 1 :: any");
}

TEST_CASE_FIXTURE(ATSFixture, "var_any_local")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
local x = 2
local x: any = 2, 3
local x: any, y = 1, 2
local x: number, y: any, z, h: nil = 1, nil
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 3);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::VarAnnot);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "local x: any = 2, 3");
}

TEST_CASE_FIXTURE(ATSFixture, "table_uses_any")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
    local x: any = 0
    local y: number
    local z = {x=x, y=y} -- not catching this
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 1);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::VarAnnot);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "local x: any = 0");
}

TEST_CASE_FIXTURE(ATSFixture, "typeof_any")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
    local x: any = 0
    function some1(x: typeof(x))
    end
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 2);
    LUAU_ASSERT(module->ats.typeInfo[1].code == Pattern::FuncArg);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "function some1(x: typeof(x))\n    end");
}

TEST_CASE_FIXTURE(ATSFixture, "table_type_assigned")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
    local x: { x:  any?} = {x = 1}
    local z: { x : any, y : number? } -- not catching this
    z.x = "bigfatlongstring"
    z.y = nil
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 2);
    LUAU_ASSERT(module->ats.typeInfo[1].code == Pattern::Assign);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "local x: { x:  any?} = {x = 1}");
}

TEST_CASE_FIXTURE(ATSFixture, "simple_func_wo_ret")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
    function some(x: any)
    end
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 1);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::FuncArg);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "function some(x: any)\n    end");
}

TEST_CASE_FIXTURE(ATSFixture, "simple_func_w_ret")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
    function other(y: number): any
    return "gotcha!"
    end
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 1);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::FuncRet);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "function other(y: number): any\n    return 'gotcha!'\n    end");
}

TEST_CASE_FIXTURE(ATSFixture, "nested_local")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
    function cool(y: number): number
    local g: any = "gratatataaa"
    return y
    end
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 1);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::VarAnnot);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "function cool(y: number): number\n    local g: any = 'gratatataaa'\n    return y\n    end");
}

TEST_CASE_FIXTURE(ATSFixture, "generic_func")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
    function reverse<T>(a: {T}, b: any): {T}
    return a
    end
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 1);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::FuncArg);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "function reverse<T>(a: {T}, b: any): {T}\n    return a\n    end");
}

TEST_CASE_FIXTURE(ATSFixture, "type_alias_any")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/Gui/Modules/A"] = R"(
    type Clear = any
    local z: Clear = "zip"  
)";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result1);

    ModulePtr module = frontend.moduleResolver.getModule("game/Gui/Modules/A");

    LUAU_ASSERT(module->ats.typeInfo.size() == 2);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::Alias);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "type Clear = any");
}

TEST_CASE_FIXTURE(ATSFixture, "multi_module_any")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

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

    ModulePtr module = frontend.moduleResolver.getModule("game/B");

    LUAU_ASSERT(module->ats.typeInfo.size() == 2);
    LUAU_ASSERT(module->ats.typeInfo[0].code == Pattern::Alias);
    LUAU_ASSERT(module->ats.typeInfo[0].node == "type Clear = any");
}

TEST_CASE_FIXTURE(ATSFixture, "cast_on_cyclic_req")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::StudioReportLuauAny2, true},
    };

    fileResolver.source["game/A"] = R"(
    local a = require(script.Parent.B) -- not resolving this module
    export type MyFunction = (number, string) -> (any)
)";

    fileResolver.source["game/B"] = R"(
    local MyFunc = require(script.Parent.A) :: any
    type Clear = any
    local z: Clear = "zip"  
)";

    CheckResult result = frontend.check("game/B");
    LUAU_REQUIRE_ERROR_COUNT(0, result);

    ModulePtr module = frontend.moduleResolver.getModule("game/B");

    LUAU_ASSERT(module->ats.typeInfo.size() == 3);
    LUAU_ASSERT(module->ats.typeInfo[1].code == Pattern::Alias);
    LUAU_ASSERT(module->ats.typeInfo[1].node == "type Clear = any");
}


TEST_SUITE_END();
