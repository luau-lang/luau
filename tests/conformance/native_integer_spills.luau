-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print("testing that native code does not hit spill limit in complex integer code")

local function bit32_add(lhs, rhs)
	return bit32.bor(lhs + rhs, 0)
end

local mem

local FUNC_LIST = {}
local GLOBAL_LIST = {}

GLOBAL_LIST[0] = { value = 4666048 }

FUNC_LIST[1] = function(loc_0, loc_1)
	local loc_2 = 0
	local loc_3 = 0
	local loc_4 = 0
	local loc_5 = 0
	local loc_6 = 0
	local loc_7 = 0
	local loc_8 = 0
	local loc_9 = 0
	local loc_10 = 0
	local loc_11 = 0
	local loc_12 = 0
	local loc_13 = 0
	local loc_14 = 0
	local loc_15 = 0
	local loc_16 = 0
	local loc_17 = 0
	local loc_18 = 0
	local loc_19 = 0
	local loc_20 = 0
	local loc_21 = 0
	local loc_22 = 0
	local loc_23 = 0
	local loc_24 = 0
	local loc_25 = 0
	local loc_26 = 0
	local loc_27 = 0
	local loc_28 = 0
	local loc_29 = 0
	local loc_30 = 0
	local loc_31 = 0
	local loc_32 = 0
	local loc_33 = 0
	local loc_34 = 0
	local loc_35 = 0
	local loc_36 = 0
	local loc_37 = 0
	local loc_38 = 0
	local loc_39 = 0
	local loc_40 = 0
	local loc_41 = 0
	local loc_42 = 0
	local loc_43 = 0
	local loc_44 = 0
	local loc_45 = 0
	local loc_46 = 0
	local loc_47 = 0
	local loc_48 = 0
	local loc_49 = 0
	local loc_50 = 0
	local loc_51 = 0
	local loc_52 = 0
	local loc_53 = 0
	local loc_54 = 0
	local loc_55 = 0
	local loc_56 = 0
	local loc_57 = 0
	local loc_58 = 0
	local loc_59 = 0
	local loc_60 = 0
	local loc_61 = 0
	local loc_62 = 0
	local loc_63 = 0
	local loc_64 = 0
	local loc_65 = 0
	local loc_66 = 0
	local loc_67 = 0
	local loc_68 = 0
	local loc_69 = 0
	local loc_70 = 0
	local loc_71 = 0
	local loc_72 = 0
	local loc_73 = 0
	local loc_74 = 0
	local loc_75 = 0
	local loc_76 = 0
	local loc_77 = 0
	local loc_78 = 0
	local reg_7
	local reg_8
	local reg_12
	local reg_13
	local reg_14
	local reg_15
	local reg_16
	while true do
		loc_2 = bit32.bor(GLOBAL_LIST[0].value - 64, 0)
		loc_3 = buffer.readu32(mem, loc_0 + 16)
		loc_4 = buffer.readu32(mem, loc_0 + 12)
		loc_5 = buffer.readu32(mem, loc_0 + 8)
		loc_6 = buffer.readu32(mem, loc_0 + 4)
		loc_7 = buffer.readu32(mem, loc_0)
		loc_8 = 0
		while true do
			loc_9 = bit32_add(loc_2, loc_8)
			loc_10 = bit32_add(loc_1, loc_8)
			buffer.writeu8(mem, bit32_add(loc_9, 3), buffer.readu8(mem, loc_10))
			buffer.writeu8(mem, bit32_add(loc_9, 2), buffer.readu8(mem, bit32_add(loc_10, 1)))
			buffer.writeu8(mem, bit32_add(loc_9, 1), buffer.readu8(mem, bit32_add(loc_10, 2)))
			buffer.writeu8(mem, loc_9, buffer.readu8(mem, bit32_add(loc_10, 3)))
			loc_8 = bit32_add(loc_8, 4)
			if loc_8 ~= 64 then
				continue
			end
			break
		end
		loc_11 = buffer.readu32(mem, loc_0)
		loc_12 = buffer.readu32(mem, loc_2 + 20)
		loc_13 = buffer.readu32(mem, loc_2 + 12)
		loc_14 = buffer.readu32(mem, loc_2 + 44)
		loc_15 = buffer.readu32(mem, loc_2 + 8)
		loc_16 = buffer.readu32(mem, loc_2)
		loc_17 = buffer.readu32(mem, loc_2 + 32)
		loc_8 = buffer.readu32(mem, loc_2 + 52)
		loc_9 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_15, loc_16), loc_17), loc_8), 1)
		loc_10 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_12, loc_13), loc_14), loc_9), 1)
		loc_18 = buffer.readu32(mem, loc_2 + 4)
		loc_19 = buffer.readu32(mem, loc_2 + 36)
		loc_1 = buffer.readu32(mem, loc_2 + 56)
		loc_20 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_13, loc_18), loc_19), loc_1), 1)
		loc_21 = buffer.readu32(mem, loc_2 + 24)
		loc_22 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_17, loc_21), loc_1), loc_10), 1)
		loc_23 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_14, loc_19), loc_20), loc_22), 1)
		loc_24 = buffer.readu32(mem, loc_2 + 40)
		loc_25 = buffer.readu32(mem, loc_2 + 28)
		loc_26 = buffer.readu32(mem, loc_2 + 16)
		loc_27 = buffer.readu32(mem, loc_2 + 60)
		loc_28 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_26, loc_15), loc_24), loc_27), 1)
		loc_29 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_25, loc_12), loc_8), loc_28), 1)
		loc_30 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_24, loc_17), loc_9), loc_29), 1)
		loc_31 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_8, loc_14), loc_10), loc_30), 1)
		loc_32 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_9, loc_1), loc_22), loc_31), 1)
		loc_33 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_10, loc_20), loc_23), loc_32), 1)
		loc_34 = buffer.readu32(mem, loc_2 + 48)
		loc_2 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_21, loc_26), loc_34), loc_20), 1)
		loc_35 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_19, loc_25), loc_27), loc_2), 1)
		loc_36 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_1, loc_34), loc_2), loc_23), 1)
		loc_37 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_20, loc_27), loc_35), loc_36), 1)
		loc_38 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_22, loc_2), loc_36), loc_33), 1)
		loc_39 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_23, loc_35), loc_37), loc_38), 1)
		loc_40 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_34, loc_24), loc_28), loc_35), 1)
		loc_41 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_27, loc_8), loc_29), loc_40), 1)
		loc_42 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_28, loc_9), loc_30), loc_41), 1)
		loc_43 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_29, loc_10), loc_31), loc_42), 1)
		loc_44 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_30, loc_22), loc_32), loc_43), 1)
		loc_45 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_31, loc_23), loc_33), loc_44), 1)
		loc_46 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_32, loc_36), loc_38), loc_45), 1)
		loc_47 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_33, loc_37), loc_39), loc_46), 1)
		loc_48 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_2, loc_28), loc_40), loc_37), 1)
		loc_49 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_35, loc_29), loc_41), loc_48), 1)
		loc_50 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_36, loc_40), loc_48), loc_39), 1)
		loc_51 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_37, loc_41), loc_49), loc_50), 1)
		loc_52 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_38, loc_48), loc_50), loc_47), 1)
		loc_53 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_39, loc_49), loc_51), loc_52), 1)
		loc_54 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_40, loc_30), loc_42), loc_49), 1)
		loc_55 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_41, loc_31), loc_43), loc_54), 1)
		loc_56 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_42, loc_32), loc_44), loc_55), 1)
		loc_57 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_43, loc_33), loc_45), loc_56), 1)
		loc_58 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_44, loc_38), loc_46), loc_57), 1)
		loc_59 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_45, loc_39), loc_47), loc_58), 1)
		loc_60 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_46, loc_50), loc_52), loc_59), 1)
		loc_61 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_47, loc_51), loc_53), loc_60), 1)
		loc_62 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_48, loc_42), loc_54), loc_51), 1)
		loc_63 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_49, loc_43), loc_55), loc_62), 1)
		loc_64 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_50, loc_54), loc_62), loc_53), 1)
		loc_65 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_51, loc_55), loc_63), loc_64), 1)
		loc_66 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_52, loc_62), loc_64), loc_61), 1)
		loc_67 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_53, loc_63), loc_65), loc_66), 1)
		loc_68 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_54, loc_44), loc_56), loc_63), 1)
		loc_69 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_55, loc_45), loc_57), loc_68), 1)
		loc_70 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_56, loc_46), loc_58), loc_69), 1)
		loc_71 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_57, loc_47), loc_59), loc_70), 1)
		loc_72 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_58, loc_52), loc_60), loc_71), 1)
		loc_73 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_59, loc_53), loc_61), loc_72), 1)
		loc_74 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_60, loc_64), loc_66), loc_73), 1)
		loc_75 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_61, loc_65), loc_67), loc_74), 1)
		loc_76 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_62, loc_56), loc_68), loc_65), 1)
		loc_77 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_64, loc_68), loc_76), loc_67), 1)
		loc_78 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_63, loc_57), loc_69), loc_76), 1)
		loc_16 = bit32_add(bit32_add(bit32_add(bit32_add(bit32.lrotate(loc_7, 5), loc_3), bit32.bxor(bit32.band(bit32.bxor(loc_4, loc_5), loc_6), loc_4)), loc_16), 1518500249)
		loc_3 = bit32.lrotate(loc_16, 30)
		loc_6 = bit32.lrotate(loc_6, 30)
		reg_15 = bit32_add(loc_3, loc_12)
		loc_12 = bit32_add(bit32_add(bit32_add(bit32_add(loc_4, bit32.bxor(bit32.band(bit32.bxor(loc_6, loc_5), loc_7), loc_5)), loc_18), bit32.lrotate(loc_16, 5)), 1518500249)
		loc_4 = bit32.lrotate(loc_7, 30)
		reg_16 = bit32_add(bit32_add(loc_6, loc_13), bit32.bxor(bit32.band(loc_12, bit32.bxor(loc_3, loc_4)), loc_4))
		loc_6 = bit32_add(bit32_add(bit32_add(bit32_add(loc_5, loc_15), bit32.bxor(bit32.band(loc_16, bit32.bxor(loc_6, loc_4)), loc_6)), bit32.lrotate(loc_12, 5)), 1518500249)
		loc_13 = bit32_add(bit32_add(reg_16, bit32.lrotate(loc_6, 5)), 1518500249)
		loc_5 = bit32.lrotate(loc_6, 30)
		loc_7 = bit32.lrotate(loc_12, 30)
		loc_3 = bit32_add(bit32_add(bit32_add(bit32_add(loc_4, loc_26), bit32.bxor(bit32.band(loc_6, bit32.bxor(loc_7, loc_3)), loc_3)), bit32.lrotate(loc_13, 5)), 1518500249)
		loc_6 = bit32_add(bit32_add(bit32_add(reg_15, bit32.bxor(bit32.band(loc_13, bit32.bxor(loc_5, loc_7)), loc_7)), bit32.lrotate(loc_3, 5)), 1518500249)
		loc_4 = bit32.lrotate(loc_6, 30)
		reg_14 = bit32_add(loc_24, loc_4)
		loc_24 = bit32.lrotate(loc_13, 30)
		reg_15 = bit32_add(loc_17, loc_24)
		loc_17 = bit32_add(bit32_add(bit32_add(bit32_add(loc_21, loc_7), bit32.bxor(bit32.band(loc_3, bit32.bxor(loc_24, loc_5)), loc_5)), bit32.lrotate(loc_6, 5)), 1518500249)
		loc_7 = bit32.lrotate(loc_3, 30)
		loc_24 = bit32_add(bit32_add(bit32_add(bit32_add(loc_25, loc_5), bit32.bxor(bit32.band(loc_6, bit32.bxor(loc_7, loc_24)), loc_24)), bit32.lrotate(loc_17, 5)), 1518500249)
		loc_3 = bit32_add(bit32_add(bit32_add(reg_15, bit32.bxor(bit32.band(loc_17, bit32.bxor(loc_4, loc_7)), loc_7)), bit32.lrotate(loc_24, 5)), 1518500249)
		loc_5 = bit32.lrotate(loc_24, 30)
		loc_17 = bit32.lrotate(loc_17, 30)
		loc_7 = bit32_add(bit32_add(bit32_add(bit32_add(loc_19, loc_7), bit32.bxor(bit32.band(loc_24, bit32.bxor(loc_17, loc_4)), loc_4)), bit32.lrotate(loc_3, 5)), 1518500249)
		loc_19 = bit32_add(bit32_add(bit32_add(reg_14, bit32.bxor(bit32.band(loc_3, bit32.bxor(loc_5, loc_17)), loc_17)), bit32.lrotate(loc_7, 5)), 1518500249)
		loc_4 = bit32.lrotate(loc_19, 30)
		reg_13 = bit32_add(loc_27, loc_4)
		loc_27 = bit32.lrotate(loc_3, 30)
		loc_14 = bit32_add(bit32_add(bit32_add(bit32_add(loc_14, loc_17), bit32.bxor(bit32.band(loc_7, bit32.bxor(loc_27, loc_5)), loc_5)), bit32.lrotate(loc_19, 5)), 1518500249)
		reg_14 = bit32_add(loc_8, loc_27)
		loc_8 = bit32.lrotate(loc_7, 30)
		loc_7 = bit32_add(bit32_add(bit32_add(bit32_add(loc_34, loc_5), bit32.bxor(bit32.band(loc_19, bit32.bxor(loc_8, loc_27)), loc_27)), bit32.lrotate(loc_14, 5)), 1518500249)
		loc_17 = bit32_add(bit32_add(bit32_add(reg_14, bit32.bxor(bit32.band(loc_14, bit32.bxor(loc_4, loc_8)), loc_8)), bit32.lrotate(loc_7, 5)), 1518500249)
		loc_5 = bit32.lrotate(loc_7, 30)
		loc_27 = bit32.lrotate(loc_14, 30)
		loc_4 = bit32_add(bit32_add(bit32_add(bit32_add(loc_1, loc_8), bit32.bxor(bit32.band(loc_7, bit32.bxor(loc_27, loc_4)), loc_4)), bit32.lrotate(loc_17, 5)), 1518500249)
		loc_7 = bit32_add(bit32_add(bit32_add(reg_13, bit32.bxor(bit32.band(loc_17, bit32.bxor(loc_5, loc_27)), loc_27)), bit32.lrotate(loc_4, 5)), 1518500249)
		loc_8 = bit32.lrotate(loc_7, 30)
		reg_12 = bit32_add(loc_2, loc_8)
		loc_2 = bit32.lrotate(loc_4, 30)
		loc_1 = bit32.lrotate(loc_17, 30)
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_9, loc_27), bit32.bxor(bit32.band(loc_4, bit32.bxor(loc_1, loc_5)), loc_5)), bit32.lrotate(loc_7, 5)), 1518500249)
		loc_5 = bit32_add(bit32_add(bit32_add(bit32_add(loc_20, loc_5), bit32.bxor(bit32.band(loc_7, bit32.bxor(loc_2, loc_1)), loc_1)), bit32.lrotate(loc_9, 5)), 1518500249)
		loc_20 = bit32.lrotate(loc_5, 30)
		loc_27 = bit32.lrotate(loc_9, 30)
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_28, loc_1), bit32.bxor(bit32.band(loc_9, bit32.bxor(loc_8, loc_2)), loc_2)), bit32.lrotate(loc_5, 5)), 1518500249)
		loc_8 = bit32_add(bit32_add(bit32_add(bit32_add(loc_10, loc_2), bit32.bxor(bit32.band(loc_5, bit32.bxor(loc_27, loc_8)), loc_8)), bit32.lrotate(loc_9, 5)), 1518500249)
		loc_10 = bit32_add(bit32_add(bit32_add(reg_12, bit32.bxor(bit32.bxor(loc_20, loc_27), loc_9)), bit32.lrotate(loc_8, 5)), 1859775393)
		loc_2 = bit32.lrotate(loc_10, 30)
		loc_1 = bit32.lrotate(loc_8, 30)
		loc_9 = bit32.lrotate(loc_9, 30)
		loc_8 = bit32_add(bit32_add(bit32_add(bit32_add(loc_29, loc_27), bit32.bxor(bit32.bxor(loc_9, loc_20), loc_8)), bit32.lrotate(loc_10, 5)), 1859775393)
		loc_10 = bit32_add(bit32_add(bit32_add(bit32_add(loc_22, loc_20), bit32.bxor(bit32.bxor(loc_1, loc_9), loc_10)), bit32.lrotate(loc_8, 5)), 1859775393)
		loc_5 = bit32.lrotate(loc_10, 30)
		loc_20 = bit32.lrotate(loc_8, 30)
		loc_8 = bit32_add(bit32_add(bit32_add(bit32_add(loc_35, loc_9), bit32.bxor(bit32.bxor(loc_2, loc_1), loc_8)), bit32.lrotate(loc_10, 5)), 1859775393)
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_30, loc_1), bit32.bxor(bit32.bxor(loc_20, loc_2), loc_10)), bit32.lrotate(loc_8, 5)), 1859775393)
		loc_10 = bit32_add(bit32_add(bit32_add(bit32_add(loc_23, loc_2), bit32.bxor(bit32.bxor(loc_5, loc_20), loc_8)), bit32.lrotate(loc_9, 5)), 1859775393)
		loc_2 = bit32.lrotate(loc_10, 30)
		loc_1 = bit32.lrotate(loc_9, 30)
		loc_8 = bit32.lrotate(loc_8, 30)
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_40, loc_20), bit32.bxor(bit32.bxor(loc_8, loc_5), loc_9)), bit32.lrotate(loc_10, 5)), 1859775393)
		loc_10 = bit32_add(bit32_add(bit32_add(bit32_add(loc_31, loc_5), bit32.bxor(bit32.bxor(loc_1, loc_8), loc_10)), bit32.lrotate(loc_9, 5)), 1859775393)
		loc_5 = bit32.lrotate(loc_10, 30)
		loc_20 = bit32.lrotate(loc_9, 30)
		loc_8 = bit32_add(bit32_add(bit32_add(bit32_add(loc_36, loc_8), bit32.bxor(bit32.bxor(loc_2, loc_1), loc_9)), bit32.lrotate(loc_10, 5)), 1859775393)
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_41, loc_1), bit32.bxor(bit32.bxor(loc_20, loc_2), loc_10)), bit32.lrotate(loc_8, 5)), 1859775393)
		loc_10 = bit32_add(bit32_add(bit32_add(bit32_add(loc_32, loc_2), bit32.bxor(bit32.bxor(loc_5, loc_20), loc_8)), bit32.lrotate(loc_9, 5)), 1859775393)
		loc_2 = bit32.lrotate(loc_10, 30)
		loc_1 = bit32.lrotate(loc_9, 30)
		loc_8 = bit32.lrotate(loc_8, 30)
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_37, loc_20), bit32.bxor(bit32.bxor(loc_8, loc_5), loc_9)), bit32.lrotate(loc_10, 5)), 1859775393)
		loc_10 = bit32_add(bit32_add(bit32_add(bit32_add(loc_42, loc_5), bit32.bxor(bit32.bxor(loc_1, loc_8), loc_10)), bit32.lrotate(loc_9, 5)), 1859775393)
		loc_5 = bit32.lrotate(loc_10, 30)
		loc_20 = bit32.lrotate(loc_9, 30)
		loc_8 = bit32_add(bit32_add(bit32_add(bit32_add(loc_33, loc_8), bit32.bxor(bit32.bxor(loc_2, loc_1), loc_9)), bit32.lrotate(loc_10, 5)), 1859775393)
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_48, loc_1), bit32.bxor(bit32.bxor(loc_20, loc_2), loc_10)), bit32.lrotate(loc_8, 5)), 1859775393)
		loc_10 = bit32_add(bit32_add(bit32_add(bit32_add(loc_43, loc_2), bit32.bxor(bit32.bxor(loc_5, loc_20), loc_8)), bit32.lrotate(loc_9, 5)), 1859775393)
		loc_2 = bit32.lrotate(loc_10, 30)
		loc_8 = bit32.lrotate(loc_8, 30)
		loc_1 = bit32.lrotate(loc_9, 30)
		loc_20 = bit32_add(bit32_add(bit32_add(bit32_add(loc_38, loc_20), bit32.bxor(bit32.bxor(loc_8, loc_5), loc_9)), bit32.lrotate(loc_10, 5)), 1859775393)
		loc_10 = bit32_add(bit32_add(bit32_add(bit32_add(loc_49, loc_5), bit32.bxor(bit32.bxor(loc_1, loc_8), loc_10)), bit32.lrotate(loc_20, 5)), 1859775393)
		loc_8 = bit32_add(bit32_add(bit32_add(bit32_add(loc_44, loc_8), bit32.bxor(bit32.bxor(loc_2, loc_1), loc_20)), bit32.lrotate(loc_10, 5)), 1859775393)
		loc_9 = bit32.lrotate(loc_10, 30)
		loc_5 = bit32.lrotate(loc_20, 30)
		loc_10 = bit32_add(bit32_add(bit32_add(bit32_add(loc_39, loc_1), bit32.bxor(bit32.bxor(loc_5, loc_2), loc_10)), bit32.lrotate(loc_8, 5)), 1859775393)
		loc_2 = bit32_add(bit32_add(bit32_add(bit32_add(loc_54, loc_2), bit32.bor(bit32.band(bit32.bor(loc_8, loc_9), loc_5), bit32.band(loc_8, loc_9))), bit32.lrotate(loc_10, 5)), 2400959708)
		loc_1 = bit32.lrotate(loc_2, 30)
		loc_8 = bit32.lrotate(loc_8, 30)
		loc_5 = bit32_add(bit32_add(bit32_add(bit32_add(loc_45, loc_5), bit32.bor(bit32.band(bit32.bor(loc_10, loc_8), loc_9), bit32.band(loc_10, loc_8))), bit32.lrotate(loc_2, 5)), 2400959708)
		loc_10 = bit32.lrotate(loc_10, 30)
		reg_8 = bit32_add(bit32_add(loc_55, loc_8), bit32.bor(bit32.band(bit32.bor(loc_5, loc_1), loc_10), bit32.band(loc_5, loc_1)))
		loc_8 = bit32_add(bit32_add(bit32_add(bit32_add(loc_50, loc_9), bit32.bor(bit32.band(bit32.bor(loc_2, loc_10), loc_8), bit32.band(loc_2, loc_10))), bit32.lrotate(loc_5, 5)), 2400959708)
		loc_9 = bit32_add(bit32_add(reg_8, bit32.lrotate(loc_8, 5)), 2400959708)
		loc_2 = bit32.lrotate(loc_8, 30)
		loc_5 = bit32.lrotate(loc_5, 30)
		loc_8 = bit32_add(bit32_add(bit32_add(bit32_add(loc_46, loc_10), bit32.bor(bit32.band(bit32.bor(loc_8, loc_5), loc_1), bit32.band(loc_8, loc_5))), bit32.lrotate(loc_9, 5)), 2400959708)
		loc_10 = bit32_add(bit32_add(bit32_add(bit32_add(loc_51, loc_1), bit32.bor(bit32.band(bit32.bor(loc_9, loc_2), loc_5), bit32.band(loc_9, loc_2))), bit32.lrotate(loc_8, 5)), 2400959708)
		loc_1 = bit32.lrotate(loc_10, 30)
		loc_9 = bit32.lrotate(loc_9, 30)
		loc_5 = bit32_add(bit32_add(bit32_add(bit32_add(loc_56, loc_5), bit32.bor(bit32.band(bit32.bor(loc_8, loc_9), loc_2), bit32.band(loc_8, loc_9))), bit32.lrotate(loc_10, 5)), 2400959708)
		loc_8 = bit32.lrotate(loc_8, 30)
		reg_7 = bit32_add(bit32_add(loc_62, loc_9), bit32.bor(bit32.band(bit32.bor(loc_5, loc_1), loc_8), bit32.band(loc_5, loc_1)))
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_47, loc_2), bit32.bor(bit32.band(bit32.bor(loc_10, loc_8), loc_9), bit32.band(loc_10, loc_8))), bit32.lrotate(loc_5, 5)), 2400959708)
		loc_10 = bit32_add(bit32_add(reg_7, bit32.lrotate(loc_9, 5)), 2400959708)
		loc_2 = bit32.lrotate(loc_9, 30)
		loc_5 = bit32.lrotate(loc_5, 30)
		loc_8 = bit32_add(bit32_add(bit32_add(bit32_add(loc_57, loc_8), bit32.bor(bit32.band(bit32.bor(loc_9, loc_5), loc_1), bit32.band(loc_9, loc_5))), bit32.lrotate(loc_10, 5)), 2400959708)
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_52, loc_1), bit32.bor(bit32.band(bit32.bor(loc_10, loc_2), loc_5), bit32.band(loc_10, loc_2))), bit32.lrotate(loc_8, 5)), 2400959708)
		loc_1 = bit32.lrotate(loc_9, 30)
		loc_10 = bit32.lrotate(loc_10, 30)
		loc_5 = bit32_add(bit32_add(bit32_add(bit32_add(loc_63, loc_5), bit32.bor(bit32.band(bit32.bor(loc_8, loc_10), loc_2), bit32.band(loc_8, loc_10))), bit32.lrotate(loc_9, 5)), 2400959708)
		loc_8 = bit32.lrotate(loc_8, 30)
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_58, loc_2), bit32.bor(bit32.band(bit32.bor(loc_9, loc_8), loc_10), bit32.band(loc_9, loc_8))), bit32.lrotate(loc_5, 5)), 2400959708)
		loc_10 = bit32_add(bit32_add(bit32_add(bit32_add(loc_53, loc_10), bit32.bor(bit32.band(bit32.bor(loc_5, loc_1), loc_8), bit32.band(loc_5, loc_1))), bit32.lrotate(loc_9, 5)), 2400959708)
		loc_2 = bit32.lrotate(loc_9, 30)
		loc_5 = bit32.lrotate(loc_5, 30)
		loc_8 = bit32_add(bit32_add(bit32_add(bit32_add(loc_68, loc_8), bit32.bor(bit32.band(bit32.bor(loc_9, loc_5), loc_1), bit32.band(loc_9, loc_5))), bit32.lrotate(loc_10, 5)), 2400959708)
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_59, loc_1), bit32.bor(bit32.band(bit32.bor(loc_10, loc_2), loc_5), bit32.band(loc_10, loc_2))), bit32.lrotate(loc_8, 5)), 2400959708)
		loc_1 = bit32.lrotate(loc_9, 30)
		loc_20 = bit32.lrotate(loc_8, 30)
		loc_10 = bit32.lrotate(loc_10, 30)
		loc_8 = bit32_add(bit32_add(bit32_add(bit32_add(loc_64, loc_5), bit32.bor(bit32.band(bit32.bor(loc_8, loc_10), loc_2), bit32.band(loc_8, loc_10))), bit32.lrotate(loc_9, 5)), 2400959708)
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_69, loc_2), bit32.bor(bit32.band(bit32.bor(loc_9, loc_20), loc_10), bit32.band(loc_9, loc_20))), bit32.lrotate(loc_8, 5)), 2400959708)
		loc_5 = bit32.lrotate(loc_9, 30)
		loc_2 = bit32.lrotate(loc_8, 30)
		loc_8 = bit32_add(bit32_add(bit32_add(bit32_add(loc_60, loc_10), bit32.bor(bit32.band(bit32.bor(loc_8, loc_1), loc_20), bit32.band(loc_8, loc_1))), bit32.lrotate(loc_9, 5)), 2400959708)
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_65, loc_20), bit32.bor(bit32.band(bit32.bor(loc_9, loc_2), loc_1), bit32.band(loc_9, loc_2))), bit32.lrotate(loc_8, 5)), 2400959708)
		loc_10 = bit32_add(bit32_add(bit32_add(bit32_add(loc_70, loc_1), bit32.bxor(bit32.bxor(loc_5, loc_2), loc_8)), bit32.lrotate(loc_9, 5)), 3395469782)
		loc_1 = bit32.lrotate(loc_10, 30)
		loc_20 = bit32.lrotate(loc_9, 30)
		loc_8 = bit32.lrotate(loc_8, 30)
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_61, loc_2), bit32.bxor(bit32.bxor(loc_8, loc_5), loc_9)), bit32.lrotate(loc_10, 5)), 3395469782)
		loc_10 = bit32_add(bit32_add(bit32_add(bit32_add(loc_76, loc_5), bit32.bxor(bit32.bxor(loc_20, loc_8), loc_10)), bit32.lrotate(loc_9, 5)), 3395469782)
		loc_2 = bit32.lrotate(loc_10, 30)
		loc_5 = bit32.lrotate(loc_9, 30)
		loc_8 = bit32_add(bit32_add(bit32_add(bit32_add(loc_71, loc_8), bit32.bxor(bit32.bxor(loc_1, loc_20), loc_9)), bit32.lrotate(loc_10, 5)), 3395469782)
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_66, loc_20), bit32.bxor(bit32.bxor(loc_5, loc_1), loc_10)), bit32.lrotate(loc_8, 5)), 3395469782)
		loc_10 = bit32_add(bit32_add(bit32_add(bit32_add(loc_78, loc_1), bit32.bxor(bit32.bxor(loc_2, loc_5), loc_8)), bit32.lrotate(loc_9, 5)), 3395469782)
		loc_1 = bit32.lrotate(loc_10, 30)
		loc_20 = bit32.lrotate(loc_9, 30)
		loc_8 = bit32.lrotate(loc_8, 30)
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_72, loc_5), bit32.bxor(bit32.bxor(loc_8, loc_2), loc_9)), bit32.lrotate(loc_10, 5)), 3395469782)
		loc_10 = bit32_add(bit32_add(bit32_add(bit32_add(loc_67, loc_2), bit32.bxor(bit32.bxor(loc_20, loc_8), loc_10)), bit32.lrotate(loc_9, 5)), 3395469782)
		loc_2 = bit32.lrotate(loc_10, 30)
		loc_5 = bit32.lrotate(loc_9, 30)
		loc_22 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_68, loc_58), loc_70), loc_78), 1)
		loc_8 = bit32_add(bit32_add(bit32_add(bit32_add(loc_22, loc_8), bit32.bxor(bit32.bxor(loc_1, loc_20), loc_9)), bit32.lrotate(loc_10, 5)), 3395469782)
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_73, loc_20), bit32.bxor(bit32.bxor(loc_5, loc_1), loc_10)), bit32.lrotate(loc_8, 5)), 3395469782)
		loc_10 = bit32_add(bit32_add(bit32_add(bit32_add(loc_77, loc_1), bit32.bxor(bit32.bxor(loc_2, loc_5), loc_8)), bit32.lrotate(loc_9, 5)), 3395469782)
		loc_1 = bit32.lrotate(loc_10, 30)
		loc_20 = bit32.lrotate(loc_9, 30)
		loc_8 = bit32.lrotate(loc_8, 30)
		loc_23 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_69, loc_59), loc_71), loc_22), 1)
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_23, loc_5), bit32.bxor(bit32.bxor(loc_8, loc_2), loc_9)), bit32.lrotate(loc_10, 5)), 3395469782)
		loc_10 = bit32_add(bit32_add(bit32_add(bit32_add(loc_74, loc_2), bit32.bxor(bit32.bxor(loc_20, loc_8), loc_10)), bit32.lrotate(loc_9, 5)), 3395469782)
		loc_2 = bit32.lrotate(loc_10, 30)
		loc_5 = bit32.lrotate(loc_9, 30)
		loc_27 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_65, loc_69), loc_78), loc_77), 1)
		loc_8 = bit32_add(bit32_add(bit32_add(bit32_add(loc_27, loc_8), bit32.bxor(bit32.bxor(loc_1, loc_20), loc_9)), bit32.lrotate(loc_10, 5)), 3395469782)
		loc_28 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_70, loc_60), loc_72), loc_23), 1)
		loc_9 = bit32_add(bit32_add(bit32_add(bit32_add(loc_28, loc_20), bit32.bxor(bit32.bxor(loc_5, loc_1), loc_10)), bit32.lrotate(loc_8, 5)), 3395469782)
		loc_10 = bit32_add(bit32_add(bit32_add(bit32_add(loc_75, loc_1), bit32.bxor(bit32.bxor(loc_2, loc_5), loc_8)), bit32.lrotate(loc_9, 5)), 3395469782)
		loc_1 = bit32.lrotate(loc_10, 30)
		buffer.writeu32(mem, loc_0 + 16, bit32_add(loc_1, buffer.readu32(mem, loc_0 + 16)))
		loc_20 = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_76, loc_70), loc_22), loc_27), 1)
		loc_8 = bit32.lrotate(loc_8, 30)
		loc_5 = bit32_add(bit32_add(bit32_add(bit32_add(loc_20, loc_5), bit32.bxor(bit32.bxor(loc_8, loc_2), loc_9)), bit32.lrotate(loc_10, 5)), 3395469782)
		loc_22 = bit32.lrotate(loc_5, 30)
		buffer.writeu32(mem, loc_0 + 12, bit32_add(loc_22, buffer.readu32(mem, loc_0 + 12)))
		loc_9 = bit32.lrotate(loc_9, 30)
		loc_10 = bit32_add(bit32_add(bit32_add(bit32_add(bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_71, loc_61), loc_73), loc_28), 1), loc_2), bit32.bxor(bit32.bxor(loc_9, loc_8), loc_10)), bit32.lrotate(loc_5, 5)), 3395469782)
		buffer.writeu32(mem, loc_0 + 8, bit32_add(bit32.lrotate(loc_10, 30), buffer.readu32(mem, loc_0 + 8)))
		loc_8 = bit32_add(bit32_add(bit32_add(bit32_add(bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_66, loc_76), loc_77), loc_75), 1), loc_8), bit32.bxor(bit32.bxor(loc_1, loc_9), loc_5)), bit32.lrotate(loc_10, 5)), 3395469782)
		buffer.writeu32(mem, loc_0 + 4, bit32_add(loc_8, buffer.readu32(mem, loc_0 + 4)))
		buffer.writeu32(mem, loc_0, bit32_add(bit32_add(bit32_add(bit32_add(bit32_add(loc_11, bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(loc_78, loc_71), loc_23), loc_20), 1)), loc_9), bit32.bxor(bit32.bxor(loc_22, loc_1), loc_10)), bit32.lrotate(loc_8, 5)), 3395469782))
		break
	end
end

FUNC_LIST[2] = function(hashtable: buffer, entries: buffer, px: buffer, py: buffer, N: number, mult: number, size: number)
    local function hash(x: number, y: number, mult: number, size: number)
        local cx, cy = math.floor(x * mult), math.floor(y * mult)
        local h = bit32.bxor((cx * 92837111), (cy * 689287499))
        return math.abs(h) % size;
    end

    for i = 0, N-1, 8 do
        local x0, x1, x2, x3, x4, x5, x6, x7 = 
            buffer.readf32(px, (i) * 4), 
            buffer.readf32(px, (i+1) * 4), 
            buffer.readf32(px, (i+2) * 4), 
            buffer.readf32(px, (i+3) * 4),
            buffer.readf32(px, (i+4) * 4), 
            buffer.readf32(px, (i+5) * 4), 
            buffer.readf32(px, (i+6) * 4), 
            buffer.readf32(px, (i+7) * 4)

        local y0, y1, y2, y3, y4, y5, y6, y7 = 
            buffer.readf32(py, (i)) * 4, 
            buffer.readf32(py, (i+1) * 4),
            buffer.readf32(py, (i+2) * 4), 
            buffer.readf32(py, (i+3) * 4),
            buffer.readf32(py, (i+4) * 4), 
            buffer.readf32(py, (i+5) * 4), 
            buffer.readf32(py, (i+6) * 4), 
            buffer.readf32(py, (i+7) * 4)
        
        local h0, h1, h2, h3, h4, h5, h6, h7 = 
            hash(x0, y0, mult, size),
            hash(x1, y1, mult, size),
            hash(x2, y2, mult, size),
            hash(x3, y3, mult, size),
            hash(x4, y4, mult, size),
            hash(x5, y5, mult, size),
            hash(x6, y6, mult, size),
            hash(x7, y7, mult, size)
        
        local vdecr0, vdecr1, vdecr2, vdecr3, vdecr4, vdecr5, vdecr6, vdecr7 =
            buffer.readu32(hashtable, h0 * 4) - 1,
            buffer.readu32(hashtable, h1 * 4) - 1,
            buffer.readu32(hashtable, h2 * 4) - 1,
            buffer.readu32(hashtable, h3 * 4) - 1,
            buffer.readu32(hashtable, h4 * 4) - 1,
            buffer.readu32(hashtable, h5 * 4) - 1,
            buffer.readu32(hashtable, h6 * 4) - 1,
            buffer.readu32(hashtable, h7 * 4) - 1
        
        buffer.writeu32(entries, vdecr0 * 4, i)
        buffer.writeu32(entries, vdecr1 * 4, i + 1)
        buffer.writeu32(entries, vdecr2 * 4, i + 2)
        buffer.writeu32(entries, vdecr3 * 4, i + 3)
        buffer.writeu32(entries, vdecr4 * 4, i + 4)
        buffer.writeu32(entries, vdecr5 * 4, i + 5)
        buffer.writeu32(entries, vdecr6 * 4, i + 6)
        buffer.writeu32(entries, vdecr7 * 4, i + 7)
        
        buffer.writeu32(hashtable, h0 * 4, vdecr0)
        buffer.writeu32(hashtable, h1 * 4, vdecr1)
        buffer.writeu32(hashtable, h2 * 4, vdecr2)
        buffer.writeu32(hashtable, h3 * 4, vdecr3)
        buffer.writeu32(hashtable, h4 * 4, vdecr4)
        buffer.writeu32(hashtable, h5 * 4, vdecr5)
        buffer.writeu32(hashtable, h6 * 4, vdecr6)
        buffer.writeu32(hashtable, h7 * 4, vdecr7)
    end
end

mem = buffer.create(1024 * 1024)

return('OK')
