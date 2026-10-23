local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

-- SAT Solver Benchmark: CDCL (Conflict-Driven Clause Learning)
-- Implements a modern SAT solver with two-watched literals, VSIDS, 1-UIP learning,
-- non-chronological backtracking, restarts, clause cleanup, and phase saving.
-- Target runtimes: Luau (lute), Lua 5.5, LuaJIT.

local math_floor = math.floor
local math_abs = math.abs
local math_max = math.max
local math_min = math.min
local string_sub = string.sub
local string_find = string.find
local string_match = string.match
local table_insert = table.insert
local table_remove = table.remove
local os_clock = os.clock

-- ============================================================================
-- Deterministic PRNG
-- ============================================================================

local prng_state = 42

function prng_reset()
    prng_state = 42
end

function prng_next()
    prng_state = (prng_state * 1103515245 + 12345) % 2147483648
    return prng_state
end

function prng_float()
    return prng_next() / 2147483648
end

function prng_range(lo, hi)
    return lo + math_floor(prng_float() * (hi - lo + 1))
end

-- ============================================================================
-- DIMACS CNF Parser
-- ============================================================================

function parse_dimacs(text)
    local num_vars = 0
    local num_clauses = 0
    local clauses = {}
    local current_clause = {}
    local pos = 1
    local len = #text

    while pos <= len do
        -- Skip whitespace
        while pos <= len do
            local ch = string_sub(text, pos, pos)
            if ch == " " or ch == "\t" or ch == "\r" then
                pos = pos + 1
            else
                break
            end
        end
        if pos > len then break end

        local ch = string_sub(text, pos, pos)
        if ch == "\n" then
            pos = pos + 1
        elseif ch == "c" then
            -- Comment line, skip to newline
            while pos <= len and string_sub(text, pos, pos) ~= "\n" do
                pos = pos + 1
            end
            if pos <= len then pos = pos + 1 end
        elseif ch == "p" then
            -- Problem line
            local line_end = string_find(text, "\n", pos) or (len + 1)
            local line = string_sub(text, pos, line_end - 1)
            local nv, nc = string_match(line, "p%s+cnf%s+(%d+)%s+(%d+)")
            if nv then
                num_vars = tonumber(nv)
                num_clauses = tonumber(nc)
            end
            pos = line_end + 1
        elseif ch == "%" then
            -- End marker (some DIMACS files)
            break
        else
            -- Clause data: read integers
            local neg = false
            if ch == "-" then
                neg = true
                pos = pos + 1
            end
            local num_start = pos
            while pos <= len do
                local c = string_sub(text, pos, pos)
                if c >= "0" and c <= "9" then
                    pos = pos + 1
                else
                    break
                end
            end
            if pos > num_start then
                local val = tonumber(string_sub(text, num_start, pos - 1))
                if neg then val = -val end
                if val == 0 then
                    if #current_clause > 0 then
                        table_insert(clauses, current_clause)
                        current_clause = {}
                    end
                else
                    table_insert(current_clause, val)
                end
            else
                pos = pos + 1
            end
        end
    end
    -- Handle clause not terminated by 0
    if #current_clause > 0 then
        table_insert(clauses, current_clause)
    end

    return num_vars, clauses
end

-- ============================================================================
-- Constants
-- ============================================================================

local UNDEF = 0
local TRUE_VAL = 1
local FALSE_VAL = -1

-- ============================================================================
-- Solver State Creation
-- ============================================================================

function solver_new(num_vars, clauses)
    local s = {}
    s.num_vars = num_vars
    s.num_original_clauses = #clauses

    -- Variable assignments: 0=undef, 1=true, -1=false
    s.assigns = {}
    for i = 1, num_vars do
        s.assigns[i] = UNDEF
    end

    -- Decision level for each variable
    s.level = {}
    for i = 1, num_vars do
        s.level[i] = -1
    end

    -- Reason clause for each variable (nil if decision)
    s.reason = {}

    -- Trail: ordered list of assignments
    s.trail = {}
    s.trail_lim = {} -- trail_lim[dl] = index in trail where decision level dl starts

    -- Current decision level
    s.decision_level = 0

    -- Clause database
    s.clauses = {}
    s.learned = {}

    -- Watch lists: for each literal, list of clause indices watching it
    -- Literal encoding: var > 0 => 2*var, var < 0 => 2*(-var)+1
    s.watches = {}
    for i = 1, 2 * num_vars + 1 do
        s.watches[i] = {}
    end

    -- VSIDS activity scores
    s.activity = {}
    for i = 1, num_vars do
        s.activity[i] = 0.0
    end
    s.var_inc = 1.0
    s.var_decay = 0.95

    -- Phase saving
    s.phase = {}
    for i = 1, num_vars do
        s.phase[i] = FALSE_VAL
    end

    -- Clause activities (for learned clause cleanup)
    s.clause_activity = {}
    s.clause_inc = 1.0
    s.clause_decay = 0.999

    -- Propagation queue pointer
    s.qhead = 1

    -- Statistics
    s.conflicts = 0
    s.decisions = 0
    s.propagations = 0
    s.restarts = 0
    s.learned_removed = 0

    -- Restart parameters (geometric)
    s.restart_base = 100
    s.restart_mult = 1.5
    s.next_restart = 100

    -- Cleanup parameters
    s.cleanup_interval = 500
    s.next_cleanup = 500

    -- Seen array for conflict analysis
    s.seen = {}
    for i = 1, num_vars do
        s.seen[i] = false
    end

    -- Add original clauses
    for i = 1, #clauses do
        local ok = solver_add_clause(s, clauses[i], false)
        if not ok then
            s.conflict_at_root = true
            return s
        end
    end

    s.conflict_at_root = false
    return s
end

-- ============================================================================
-- Literal helpers
-- ============================================================================

function lit_var(lit)
    if lit > 0 then return lit else return -lit end
end

function lit_sign(lit)
    if lit > 0 then return TRUE_VAL else return FALSE_VAL end
end

function lit_index(lit)
    if lit > 0 then return 2 * lit else return 2 * (-lit) + 1 end
end

function lit_neg(lit)
    return -lit
end

function lit_value(s, lit)
    local v = lit_var(lit)
    local a = s.assigns[v]
    if a == UNDEF then return UNDEF end
    if lit > 0 then return a else return -a end
end

-- ============================================================================
-- Clause addition
-- ============================================================================

function solver_add_clause(s, lits, is_learned)
    -- Remove false literals at level 0, detect tautologies
    local cleaned = {}
    local seen_lits = {}

    for i = 1, #lits do
        local l = lits[i]
        local val = lit_value(s, l)
        -- At level 0, skip falsified literals
        if s.decision_level == 0 and val == FALSE_VAL then
            -- skip
        elseif seen_lits[-l] then
            -- Tautology
            return true
        elseif not seen_lits[l] then
            seen_lits[l] = true
            table_insert(cleaned, l)
        end
    end

    if #cleaned == 0 then
        return false -- Empty clause = conflict
    end

    if #cleaned == 1 then
        -- Unit clause: enqueue
        return solver_enqueue(s, cleaned[1], nil)
    end

    -- Create clause record
    local clause = {}
    clause.lits = cleaned
    clause.is_learned = is_learned
    clause.activity = 0.0
    clause.lbd = 0

    -- For watched literals, put the two best literals first
    -- (highest decision level or undefined)
    if is_learned then
        -- Put asserting literal first, highest level second
        local max_level = -1
        local max_idx = 2
        for i = 2, #cleaned do
            local v = lit_var(cleaned[i])
            if s.level[v] > max_level then
                max_level = s.level[v]
                max_idx = i
            end
        end
        if max_idx ~= 2 then
            cleaned[2], cleaned[max_idx] = cleaned[max_idx], cleaned[2]
        end
    end

    local ci
    if is_learned then
        table_insert(s.learned, clause)
        ci = { learned = true, idx = #s.learned }
        s.clause_activity[#s.learned] = s.clause_inc
        clause.lbd = compute_lbd(s, cleaned)
    else
        table_insert(s.clauses, clause)
        ci = { learned = false, idx = #s.clauses }
    end

    -- Add watches on first two literals
    local w1 = lit_index(cleaned[1])
    local w2 = lit_index(cleaned[2])
    table_insert(s.watches[w1], ci)
    table_insert(s.watches[w2], ci)

    return true
end

-- ============================================================================
-- Compute LBD (Literal Block Distance) for a clause
-- ============================================================================

function compute_lbd(s, lits)
    local levels = {}
    local count = 0
    for i = 1, #lits do
        local v = lit_var(lits[i])
        local lv = s.level[v]
        if lv > 0 and not levels[lv] then
            levels[lv] = true
            count = count + 1
        end
    end
    return count
end

-- ============================================================================
-- Assignment and trail management
-- ============================================================================

function solver_enqueue(s, lit, reason_clause)
    local v = lit_var(lit)
    if s.assigns[v] ~= UNDEF then
        -- Already assigned; check consistency
        if lit_value(s, lit) == FALSE_VAL then
            return false
        end
        return true
    end

    s.assigns[v] = lit_sign(lit)
    s.level[v] = s.decision_level
    s.reason[v] = reason_clause
    s.phase[v] = lit_sign(lit)
    table_insert(s.trail, lit)
    return true
end

function solver_new_decision_level(s)
    s.decision_level = s.decision_level + 1
    s.trail_lim[s.decision_level] = #s.trail + 1
end

function solver_backtrack(s, target_level)
    if s.decision_level <= target_level then return end

    local backtrack_point = s.trail_lim[target_level + 1]
    if not backtrack_point then backtrack_point = 1 end

    for i = #s.trail, backtrack_point, -1 do
        local lit = s.trail[i]
        local v = lit_var(lit)
        s.assigns[v] = UNDEF
        s.level[v] = -1
        s.reason[v] = nil
        s.trail[i] = nil
    end

    -- Reset propagation queue
    s.qhead = backtrack_point

    -- Remove trail_lim entries above target_level
    for i = s.decision_level, target_level + 1, -1 do
        s.trail_lim[i] = nil
    end

    s.decision_level = target_level
end

-- ============================================================================
-- Two-Watched-Literal Propagation (BCP)
-- ============================================================================

function solver_propagate(s)
    while s.qhead <= #s.trail do
        local p = s.trail[s.qhead]
        s.qhead = s.qhead + 1
        s.propagations = s.propagations + 1

        -- p was assigned true, so ~p is false => look at watches of ~p
        local false_lit = lit_neg(p)
        local wi = lit_index(false_lit)
        local watch_list = s.watches[wi]

        local new_watch_list = {}
        local conflict_clause = nil
        local j = 1
        local wlen = #watch_list

        while j <= wlen do
            local ci = watch_list[j]
            local clause = get_clause(s, ci)
            if not clause then
                -- Clause was removed
                j = j + 1
            else
                local lits = clause.lits

                -- Make sure false_lit is lits[2]
                if lits[1] == false_lit then
                    lits[1], lits[2] = lits[2], lits[1]
                end

                -- Check if first watched literal is already true
                local first_val = lit_value(s, lits[1])
                if first_val == TRUE_VAL then
                    table_insert(new_watch_list, ci)
                    j = j + 1
                else
                    -- Look for a new literal to watch
                    local found_new = false
                    for k = 3, #lits do
                        local lk_val = lit_value(s, lits[k])
                        if lk_val ~= FALSE_VAL then
                            -- Swap lits[2] and lits[k]
                            lits[2], lits[k] = lits[k], lits[2]
                            -- Add watch for new lits[2]
                            local new_wi = lit_index(lits[2])
                            table_insert(s.watches[new_wi], ci)
                            found_new = true
                            break
                        end
                    end

                    if found_new then
                        -- This watch is no longer here
                        j = j + 1
                    else
                        -- No new watch found
                        table_insert(new_watch_list, ci)
                        if first_val == FALSE_VAL then
                            -- Conflict!
                            conflict_clause = ci
                            -- Copy remaining watches
                            j = j + 1
                            while j <= wlen do
                                table_insert(new_watch_list, watch_list[j])
                                j = j + 1
                            end
                        else
                            -- Unit propagation
                            local ok = solver_enqueue(s, lits[1], ci)
                            if not ok then
                                conflict_clause = ci
                                j = j + 1
                                while j <= wlen do
                                    table_insert(new_watch_list, watch_list[j])
                                    j = j + 1
                                end
                            else
                                j = j + 1
                            end
                        end
                    end
                end
            end
        end

        s.watches[wi] = new_watch_list

        if conflict_clause then
            return conflict_clause
        end
    end

    return nil -- No conflict
end

-- ============================================================================
-- Get clause from clause index
-- ============================================================================

function get_clause(s, ci)
    if ci.learned then
        return s.learned[ci.idx]
    else
        return s.clauses[ci.idx]
    end
end

-- ============================================================================
-- VSIDS: Variable activity
-- ============================================================================

function var_bump_activity(s, v)
    s.activity[v] = s.activity[v] + s.var_inc
    if s.activity[v] > 1e100 then
        -- Rescale
        for i = 1, s.num_vars do
            s.activity[i] = s.activity[i] * 1e-100
        end
        s.var_inc = s.var_inc * 1e-100
    end
end

function var_decay_activity(s)
    s.var_inc = s.var_inc / s.var_decay
end

-- ============================================================================
-- Clause activity
-- ============================================================================

function clause_bump_activity(s, ci)
    if ci.learned then
        local old = s.clause_activity[ci.idx] or 0
        s.clause_activity[ci.idx] = old + s.clause_inc
        if s.clause_activity[ci.idx] > 1e20 then
            for i = 1, #s.learned do
                s.clause_activity[i] = (s.clause_activity[i] or 0) * 1e-20
            end
            s.clause_inc = s.clause_inc * 1e-20
        end
    end
end

function clause_decay_activity(s)
    s.clause_inc = s.clause_inc / s.clause_decay
end

-- ============================================================================
-- Decision: VSIDS heuristic with phase saving
-- ============================================================================

function solver_pick_decision(s)
    local best_var = -1
    local best_act = -1.0

    for v = 1, s.num_vars do
        if s.assigns[v] == UNDEF then
            if s.activity[v] > best_act then
                best_act = s.activity[v]
                best_var = v
            end
        end
    end

    if best_var == -1 then
        return 0 -- All assigned
    end

    -- Use phase saving
    local pol = s.phase[best_var]
    if pol == TRUE_VAL then
        return best_var
    else
        return -best_var
    end
end

-- ============================================================================
-- Conflict analysis: 1-UIP scheme
-- ============================================================================

function solver_analyze(s, conflict_ci)
    local learned_lits = {}
    local counter = 0
    local p = nil
    local p_reason = conflict_ci

    -- Clear seen
    -- (already cleared from prior call)

    local btlevel = 0
    local trail_idx = #s.trail

    repeat
        -- Process reason clause
        local clause = get_clause(s, p_reason)
        if clause then
            clause_bump_activity(s, p_reason)
            local start_idx = 1
            if p then start_idx = 1 end

            local lits = clause.lits
            for i = 1, #lits do
                local lit = lits[i]
                local v = lit_var(lit)
                if v ~= (p and lit_var(p) or 0) and not s.seen[v] then
                    if s.level[v] == 0 then
                        -- Level 0 literals are always false, skip
                    elseif s.level[v] >= s.decision_level then
                        s.seen[v] = true
                        counter = counter + 1
                        var_bump_activity(s, v)
                    else
                        s.seen[v] = true
                        table_insert(learned_lits, lit_neg(lit))
                        var_bump_activity(s, v)
                        if s.level[v] > btlevel then
                            btlevel = s.level[v]
                        end
                    end
                end
            end
        end

        -- Find next literal on trail at current decision level
        repeat
            p = s.trail[trail_idx]
            trail_idx = trail_idx - 1
        until s.seen[lit_var(p)]

        counter = counter - 1
        s.seen[lit_var(p)] = false

        if counter > 0 then
            p_reason = s.reason[lit_var(p)]
            if not p_reason then
                -- This shouldn't happen in a correct solver, but safeguard
                break
            end
        end
    until counter <= 0

    -- The 1-UIP literal
    local uip_lit = lit_neg(p)
    -- Insert at front
    table_insert(learned_lits, 1, uip_lit)

    -- Clear seen flags
    for i = 1, #learned_lits do
        s.seen[lit_var(learned_lits[i])] = false
    end

    -- Minimize learned clause (simple self-subsumption)
    learned_lits = minimize_clause(s, learned_lits)

    -- Determine backtrack level
    if #learned_lits == 1 then
        btlevel = 0
    else
        -- Find second highest level
        local max_i = 2
        for i = 3, #learned_lits do
            local v = lit_var(learned_lits[i])
            local vi = lit_var(learned_lits[max_i])
            if s.level[v] > s.level[vi] then
                max_i = i
            end
        end
        -- Swap
        learned_lits[2], learned_lits[max_i] = learned_lits[max_i], learned_lits[2]
        btlevel = s.level[lit_var(learned_lits[2])]
    end

    var_decay_activity(s)
    clause_decay_activity(s)

    return learned_lits, btlevel
end

-- ============================================================================
-- Clause minimization
-- ============================================================================

function minimize_clause(s, lits)
    if #lits <= 2 then return lits end

    local dominated = {}
    for i = 2, #lits do
        local v = lit_var(lits[i])
        local r = s.reason[v]
        if r then
            local rc = get_clause(s, r)
            if rc then
                local dominated_flag = true
                local rlits = rc.lits
                for j = 1, #rlits do
                    local rv = lit_var(rlits[j])
                    if rv ~= v then
                        if not s.seen[rv] and s.level[rv] > 0 then
                            dominated_flag = false
                            break
                        end
                    end
                end
                if dominated_flag then
                    dominated[i] = true
                end
            end
        end
    end

    local result = { lits[1] }
    for i = 2, #lits do
        if not dominated[i] then
            table_insert(result, lits[i])
        end
    end
    return result
end

-- ============================================================================
-- Learned clause cleanup
-- ============================================================================

function solver_reduce_db(s)
    local n = #s.learned
    if n < 10 then return end

    -- Sort learned clauses by activity (keep high activity)
    local indices = {}
    for i = 1, n do
        indices[i] = i
    end

    -- Simple selection: remove bottom half by activity
    local limit = math_floor(n / 2)
    local threshold = 0.0
    -- Find median activity approximately
    local sum_act = 0.0
    for i = 1, n do
        sum_act = sum_act + (s.clause_activity[i] or 0)
    end
    threshold = sum_act / n

    local to_remove = {}
    local removed_count = 0
    for i = 1, n do
        local clause = s.learned[i]
        if clause then
            local act = s.clause_activity[i] or 0
            -- Don't remove short clauses (LBD <= 2) or locked clauses
            if act < threshold and clause.lbd > 2 and not is_clause_locked(s, i) then
                if removed_count < limit then
                    to_remove[i] = true
                    removed_count = removed_count + 1
                end
            end
        end
    end

    -- Remove clauses
    for idx in next, to_remove do
        remove_learned_clause(s, idx)
    end

    s.learned_removed = s.learned_removed + removed_count
end

function is_clause_locked(s, learned_idx)
    local clause = s.learned[learned_idx]
    if not clause then return false end
    local lits = clause.lits
    if #lits == 0 then return false end
    local v = lit_var(lits[1])
    local r = s.reason[v]
    if r and r.learned and r.idx == learned_idx then
        return true
    end
    return false
end

function remove_learned_clause(s, learned_idx)
    -- Mark as nil (watches will skip nil clauses)
    s.learned[learned_idx] = nil
    s.clause_activity[learned_idx] = 0
end

-- ============================================================================
-- Restart
-- ============================================================================

function solver_should_restart(s)
    return s.conflicts >= s.next_restart
end

function solver_do_restart(s)
    s.restarts = s.restarts + 1
    solver_backtrack(s, 0)
    s.next_restart = math_floor(s.next_restart * s.restart_mult)
end

-- ============================================================================
-- Main CDCL solve loop
-- ============================================================================

function solver_solve(s)
    if s.conflict_at_root then
        return "UNSAT", nil
    end

    -- Initial propagation
    local conf = solver_propagate(s)
    if conf then
        return "UNSAT", nil
    end

    while true do
        -- Check restart
        if solver_should_restart(s) then
            solver_do_restart(s)
        end

        -- Check cleanup
        if s.conflicts >= s.next_cleanup then
            solver_reduce_db(s)
            s.next_cleanup = s.next_cleanup + s.cleanup_interval
        end

        -- Decide
        local lit = solver_pick_decision(s)
        if lit == 0 then
            -- All variables assigned => SAT
            local assignment = {}
            for v = 1, s.num_vars do
                assignment[v] = s.assigns[v]
            end
            return "SAT", assignment
        end

        s.decisions = s.decisions + 1
        solver_new_decision_level(s)
        solver_enqueue(s, lit, nil)

        -- Propagate
        local conflict = solver_propagate(s)

        while conflict do
            s.conflicts = s.conflicts + 1

            if s.decision_level == 0 then
                return "UNSAT", nil
            end

            -- Analyze conflict
            local learned_lits, btlevel = solver_analyze(s, conflict)

            -- Backtrack
            solver_backtrack(s, btlevel)

            -- Add learned clause
            if #learned_lits == 1 then
                -- Unit clause at level 0
                solver_enqueue(s, learned_lits[1], nil)
            else
                -- Create new clause
                local clause = {}
                clause.lits = learned_lits
                clause.is_learned = true
                clause.activity = s.clause_inc
                clause.lbd = compute_lbd(s, learned_lits)

                table_insert(s.learned, clause)
                local ci = { learned = true, idx = #s.learned }
                s.clause_activity[#s.learned] = s.clause_inc

                -- Watch first two literals
                local w1 = lit_index(learned_lits[1])
                local w2 = lit_index(learned_lits[2])
                table_insert(s.watches[w1], ci)
                table_insert(s.watches[w2], ci)

                -- Assert the first literal (it's the UIP)
                solver_enqueue(s, learned_lits[1], ci)
            end

            -- Propagate again
            conflict = solver_propagate(s)
        end
    end
end

-- ============================================================================
-- Verify SAT assignment
-- ============================================================================

function verify_sat(num_vars, clauses, assignment)
    for i = 1, #clauses do
        local clause = clauses[i]
        local satisfied = false
        for j = 1, #clause do
            local lit = clause[j]
            local v = lit_var(lit)
            if v <= num_vars then
                local a = assignment[v]
                if (lit > 0 and a == TRUE_VAL) or (lit < 0 and a == FALSE_VAL) then
                    satisfied = true
                    break
                end
            end
        end
        if not satisfied then
            return false, i
        end
    end
    return true, 0
end

-- ============================================================================
-- Compute checksum of assignment
-- ============================================================================

function assignment_checksum(assignment, num_vars)
    local sum = 0
    for i = 1, num_vars do
        local val = assignment[i] or 0
        -- Mix bits
        if val == TRUE_VAL then
            sum = sum + i * 7919
        elseif val == FALSE_VAL then
            sum = sum + i * 104729
        end
        sum = sum % 1000000007
    end
    return sum
end

-- ============================================================================
-- Random 3-SAT generator (near phase transition ratio ~4.26)
-- ============================================================================

function generate_random_3sat(num_vars, num_clauses)
    local clauses = {}
    for i = 1, num_clauses do
        local clause = {}
        local vars_used = {}
        local j = 0
        while j < 3 do
            local v = prng_range(1, num_vars)
            if not vars_used[v] then
                vars_used[v] = true
                if prng_float() < 0.5 then
                    table_insert(clause, v)
                else
                    table_insert(clause, -v)
                end
                j = j + 1
            end
        end
        table_insert(clauses, clause)
    end
    return clauses
end

-- ============================================================================
-- Convert clauses to DIMACS string (for internal consistency)
-- ============================================================================

function clauses_to_dimacs(num_vars, clauses)
    local parts = {}
    table_insert(parts, "p cnf " .. num_vars .. " " .. #clauses .. "\n")
    for i = 1, #clauses do
        local line = ""
        for j = 1, #clauses[i] do
            if j > 1 then line = line .. " " end
            line = line .. clauses[i][j]
        end
        line = line .. " 0\n"
        table_insert(parts, line)
    end
    return table.concat(parts)
end

-- ============================================================================
-- Test instances: Trivially satisfiable (< 20 vars)
-- ============================================================================

local TRIVIAL_SAT_1 = [[
p cnf 5 6
1 2 3 0
-1 2 4 0
1 -3 5 0
-2 4 5 0
3 -4 -5 0
1 -2 -3 0
]]

local TRIVIAL_SAT_2 = [[
p cnf 8 10
1 2 0
-1 3 0
-2 4 0
3 5 0
-4 6 0
5 7 0
-6 8 0
1 -3 4 0
2 -5 6 0
-7 8 -1 0
]]

local TRIVIAL_SAT_3 = [[
p cnf 10 15
1 2 3 0
-1 4 5 0
2 -3 6 0
-4 5 7 0
3 6 -7 0
-2 8 9 0
4 -8 10 0
-5 9 -10 0
1 -6 7 0
-3 8 -9 0
2 5 10 0
-1 -4 6 0
7 8 9 0
-2 -6 10 0
1 3 -5 0
]]

-- ============================================================================
-- Test instances: Challenging satisfiable (50-100 vars, 200-400 clauses)
-- ============================================================================

local CHALLENGING_SAT_1 = [[
p cnf 50 213
1 -2 3 0
-4 5 -6 0
7 8 -9 0
-10 11 12 0
13 -14 15 0
-16 17 -18 0
19 20 -21 0
-22 23 24 0
25 -26 27 0
-28 29 -30 0
31 32 -33 0
-34 35 36 0
37 -38 39 0
-40 41 -42 0
43 44 -45 0
-46 47 48 0
49 -50 1 0
-2 3 -4 0
5 -6 7 0
-8 9 -10 0
11 12 -13 0
-14 15 -16 0
17 18 -19 0
-20 21 -22 0
23 24 -25 0
-26 27 -28 0
29 30 -31 0
-32 33 -34 0
35 36 -37 0
-38 39 -40 0
41 42 -43 0
-44 45 -46 0
47 48 -49 0
-50 1 -2 0
3 -4 5 0
-6 7 -8 0
9 10 -11 0
-12 13 -14 0
15 16 -17 0
-18 19 -20 0
21 22 -23 0
-24 25 -26 0
27 28 -29 0
-30 31 -32 0
33 34 -35 0
-36 37 -38 0
39 40 -41 0
-42 43 -44 0
45 46 -47 0
-48 49 -50 0
1 -3 5 0
-2 4 -6 0
7 -9 11 0
-8 10 -12 0
13 -15 17 0
-14 16 -18 0
19 -21 23 0
-20 22 -24 0
25 -27 29 0
-26 28 -30 0
31 -33 35 0
-32 34 -36 0
37 -39 41 0
-38 40 -42 0
43 -45 47 0
-44 46 -48 0
49 -1 3 0
-50 2 -4 0
5 -7 9 0
-6 8 -10 0
11 -13 15 0
-12 14 -16 0
17 -19 21 0
-18 20 -22 0
23 -25 27 0
-24 26 -28 0
29 -31 33 0
-30 32 -34 0
35 -37 39 0
-36 38 -40 0
41 -43 45 0
-42 44 -46 0
47 -49 1 0
-48 50 -2 0
1 2 -5 0
-3 4 -7 0
6 -8 9 0
-10 11 -13 0
12 -14 15 0
-16 17 -19 0
18 -20 21 0
-22 23 -25 0
24 -26 27 0
-28 29 -31 0
30 -32 33 0
-34 35 -37 0
36 -38 39 0
-40 41 -43 0
42 -44 45 0
-46 47 -49 0
48 -50 1 0
2 -4 6 0
-3 5 -7 0
8 -10 12 0
-9 11 -13 0
14 -16 18 0
-15 17 -19 0
20 -22 24 0
-21 23 -25 0
26 -28 30 0
-27 29 -31 0
32 -34 36 0
-33 35 -37 0
38 -40 42 0
-39 41 -43 0
44 -46 48 0
-45 47 -49 0
50 -1 2 0
-3 4 5 0
6 7 -8 0
-9 10 11 0
12 13 -14 0
-15 16 17 0
18 19 -20 0
-21 22 23 0
24 25 -26 0
-27 28 29 0
30 31 -32 0
-33 34 35 0
36 37 -38 0
-39 40 41 0
42 43 -44 0
-45 46 47 0
48 49 -50 0
-1 2 3 0
4 5 -6 0
-7 8 9 0
10 11 -12 0
-13 14 15 0
16 17 -18 0
-19 20 21 0
22 23 -24 0
-25 26 27 0
28 29 -30 0
-31 32 33 0
34 35 -36 0
-37 38 39 0
40 41 -42 0
-43 44 45 0
46 47 -48 0
-49 50 1 0
-1 -2 3 0
4 -5 6 0
-7 -8 9 0
10 -11 12 0
-13 -14 15 0
16 -17 18 0
-19 -20 21 0
22 -23 24 0
-25 -26 27 0
28 -29 30 0
-31 -32 33 0
34 -35 36 0
-37 -38 39 0
40 -41 42 0
-43 -44 45 0
46 -47 48 0
-49 -50 1 0
1 10 20 0
2 11 21 0
3 12 22 0
4 13 23 0
5 14 24 0
6 15 25 0
7 16 26 0
8 17 27 0
9 18 28 0
10 19 29 0
11 20 30 0
12 21 31 0
13 22 32 0
14 23 33 0
15 24 34 0
16 25 35 0
17 26 36 0
18 27 37 0
19 28 38 0
20 29 39 0
21 30 40 0
22 31 41 0
23 32 42 0
24 33 43 0
25 34 44 0
26 35 45 0
27 36 46 0
28 37 47 0
29 38 48 0
30 39 49 0
31 40 50 0
-1 -10 -20 0
-2 -11 -21 0
-3 -12 -22 0
-4 -13 -23 0
-5 -14 -24 0
-6 -15 -25 0
-7 -16 -26 0
-8 -17 -27 0
-9 -18 -28 0
-10 -19 -29 0
-11 -20 -30 0
-12 -21 -31 0
-13 -22 -32 0
]]

local CHALLENGING_SAT_2 = [[
p cnf 75 320
1 2 -3 0
-4 5 6 0
7 -8 9 0
-10 11 -12 0
13 14 -15 0
-16 17 18 0
19 -20 21 0
-22 23 -24 0
25 26 -27 0
-28 29 30 0
31 -32 33 0
-34 35 -36 0
37 38 -39 0
-40 41 42 0
43 -44 45 0
-46 47 -48 0
49 50 -51 0
-52 53 54 0
55 -56 57 0
-58 59 -60 0
61 62 -63 0
-64 65 66 0
67 -68 69 0
-70 71 -72 0
73 74 -75 0
-1 2 -4 0
3 -5 6 0
-7 8 -9 0
10 -11 12 0
-13 14 -15 0
16 -17 18 0
-19 20 -21 0
22 -23 24 0
-25 26 -27 0
28 -29 30 0
-31 32 -33 0
34 -35 36 0
-37 38 -39 0
40 -41 42 0
-43 44 -45 0
46 -47 48 0
-49 50 -51 0
52 -53 54 0
-55 56 -57 0
58 -59 60 0
-61 62 -63 0
64 -65 66 0
-67 68 -69 0
70 -71 72 0
-73 74 -75 0
1 -3 5 0
-2 4 -6 0
7 -9 11 0
-8 10 -12 0
13 -15 17 0
-14 16 -18 0
19 -21 23 0
-20 22 -24 0
25 -27 29 0
-26 28 -30 0
31 -33 35 0
-32 34 -36 0
37 -39 41 0
-38 40 -42 0
43 -45 47 0
-44 46 -48 0
49 -51 53 0
-50 52 -54 0
55 -57 59 0
-56 58 -60 0
61 -63 65 0
-62 64 -66 0
67 -69 71 0
-68 70 -72 0
73 -75 1 0
-74 2 -3 0
4 -6 8 0
-5 7 -9 0
10 -12 14 0
-11 13 -15 0
16 -18 20 0
-17 19 -21 0
22 -24 26 0
-23 25 -27 0
28 -30 32 0
-29 31 -33 0
34 -36 38 0
-35 37 -39 0
40 -42 44 0
-41 43 -45 0
46 -48 50 0
-47 49 -51 0
52 -54 56 0
-53 55 -57 0
58 -60 62 0
-59 61 -63 0
64 -66 68 0
-65 67 -69 0
70 -72 74 0
-71 73 -75 0
1 5 10 0
2 6 11 0
3 7 12 0
4 8 13 0
5 9 14 0
6 10 15 0
7 11 16 0
8 12 17 0
9 13 18 0
10 14 19 0
11 15 20 0
12 16 21 0
13 17 22 0
14 18 23 0
15 19 24 0
16 20 25 0
17 21 26 0
18 22 27 0
19 23 28 0
20 24 29 0
21 25 30 0
22 26 31 0
23 27 32 0
24 28 33 0
25 29 34 0
26 30 35 0
27 31 36 0
28 32 37 0
29 33 38 0
30 34 39 0
31 35 40 0
32 36 41 0
33 37 42 0
34 38 43 0
35 39 44 0
36 40 45 0
37 41 46 0
38 42 47 0
39 43 48 0
40 44 49 0
41 45 50 0
42 46 51 0
43 47 52 0
44 48 53 0
45 49 54 0
46 50 55 0
47 51 56 0
48 52 57 0
49 53 58 0
50 54 59 0
-1 -5 -10 0
-2 -6 -11 0
-3 -7 -12 0
-4 -8 -13 0
-5 -9 -14 0
-6 -10 -15 0
-7 -11 -16 0
-8 -12 -17 0
-9 -13 -18 0
-10 -14 -19 0
-11 -15 -20 0
-12 -16 -21 0
-13 -17 -22 0
-14 -18 -23 0
-15 -19 -24 0
-16 -20 -25 0
-17 -21 -26 0
-18 -22 -27 0
-19 -23 -28 0
-20 -24 -29 0
-21 -25 -30 0
-22 -26 -31 0
-23 -27 -32 0
-24 -28 -33 0
-25 -29 -34 0
-26 -30 -35 0
-27 -31 -36 0
-28 -32 -37 0
-29 -33 -38 0
-30 -34 -39 0
-31 -35 -40 0
-32 -36 -41 0
-33 -37 -42 0
-34 -38 -43 0
-35 -39 -44 0
-36 -40 -45 0
-37 -41 -46 0
-38 -42 -47 0
-39 -43 -48 0
-40 -44 -49 0
-41 -45 -50 0
-42 -46 -51 0
-43 -47 -52 0
-44 -48 -53 0
-45 -49 -54 0
-46 -50 -55 0
-47 -51 -56 0
-48 -52 -57 0
-49 -53 -58 0
-50 -54 -59 0
51 55 60 0
52 56 61 0
53 57 62 0
54 58 63 0
55 59 64 0
56 60 65 0
57 61 66 0
58 62 67 0
59 63 68 0
60 64 69 0
61 65 70 0
62 66 71 0
63 67 72 0
64 68 73 0
65 69 74 0
66 70 75 0
67 71 -1 0
68 72 -2 0
69 73 -3 0
70 74 -4 0
71 75 -5 0
72 -6 -7 0
73 -8 -9 0
74 -10 -11 0
75 -12 -13 0
-51 -55 -60 0
-52 -56 -61 0
-53 -57 -62 0
-54 -58 -63 0
-55 -59 -64 0
-56 -60 -65 0
-57 -61 -66 0
-58 -62 -67 0
-59 -63 -68 0
-60 -64 -69 0
-61 -65 -70 0
-62 -66 -71 0
-63 -67 -72 0
-64 -68 -73 0
-65 -69 -74 0
-66 -70 -75 0
1 20 40 0
2 21 41 0
3 22 42 0
4 23 43 0
5 24 44 0
6 25 45 0
7 26 46 0
8 27 47 0
9 28 48 0
10 29 49 0
11 30 50 0
12 31 51 0
13 32 52 0
14 33 53 0
15 34 54 0
16 35 55 0
17 36 56 0
18 37 57 0
19 38 58 0
20 39 59 0
21 40 60 0
22 41 61 0
23 42 62 0
24 43 63 0
25 44 64 0
26 45 65 0
27 46 66 0
28 47 67 0
29 48 68 0
30 49 69 0
31 50 70 0
32 51 71 0
33 52 72 0
34 53 73 0
35 54 74 0
36 55 75 0
-1 -20 -40 0
-2 -21 -41 0
-3 -22 -42 0
-4 -23 -43 0
-5 -24 -44 0
-6 -25 -45 0
-7 -26 -46 0
-8 -27 -47 0
-9 -28 -48 0
-10 -29 -49 0
-11 -30 -50 0
-12 -31 -51 0
-13 -32 -52 0
-14 -33 -53 0
-15 -34 -54 0
-16 -35 -55 0
-17 -36 -56 0
-18 -37 -57 0
-19 -38 -58 0
-20 -39 -59 0
-21 -40 -60 0
-22 -41 -61 0
-23 -42 -62 0
-24 -43 -63 0
-25 -44 -64 0
-26 -45 -65 0
-27 -46 -66 0
-28 -47 -67 0
-29 -48 -68 0
-30 -49 -69 0
-31 -50 -70 0
-32 -51 -71 0
-33 -52 -72 0
-34 -53 -73 0
-35 -54 -74 0
-36 -55 -75 0
]]

local CHALLENGING_SAT_3 = [[
p cnf 100 400
1 -2 3 0
-4 5 -6 0
7 8 -9 0
-10 11 12 0
13 -14 15 0
-16 17 -18 0
19 20 -21 0
-22 23 24 0
25 -26 27 0
-28 29 -30 0
31 32 -33 0
-34 35 36 0
37 -38 39 0
-40 41 -42 0
43 44 -45 0
-46 47 48 0
49 -50 51 0
-52 53 -54 0
55 56 -57 0
-58 59 60 0
61 -62 63 0
-64 65 -66 0
67 68 -69 0
-70 71 72 0
73 -74 75 0
-76 77 -78 0
79 80 -81 0
-82 83 84 0
85 -86 87 0
-88 89 -90 0
91 92 -93 0
-94 95 96 0
97 -98 99 0
-100 1 -2 0
3 -4 5 0
-6 7 -8 0
9 10 -11 0
-12 13 -14 0
15 16 -17 0
-18 19 -20 0
21 22 -23 0
-24 25 -26 0
27 28 -29 0
-30 31 -32 0
33 34 -35 0
-36 37 -38 0
39 40 -41 0
-42 43 -44 0
45 46 -47 0
-48 49 -50 0
51 52 -53 0
-54 55 -56 0
57 58 -59 0
-60 61 -62 0
63 64 -65 0
-66 67 -68 0
69 70 -71 0
-72 73 -74 0
75 76 -77 0
-78 79 -80 0
81 82 -83 0
-84 85 -86 0
87 88 -89 0
-90 91 -92 0
93 94 -95 0
-96 97 -98 0
99 100 -1 0
-2 3 -4 0
5 -6 7 0
-8 9 -10 0
11 12 -13 0
-14 15 -16 0
17 18 -19 0
-20 21 -22 0
23 24 -25 0
-26 27 -28 0
29 30 -31 0
-32 33 -34 0
35 36 -37 0
-38 39 -40 0
41 42 -43 0
-44 45 -46 0
47 48 -49 0
-50 51 -52 0
53 54 -55 0
-56 57 -58 0
59 60 -61 0
-62 63 -64 0
65 66 -67 0
-68 69 -70 0
71 72 -73 0
-74 75 -76 0
77 78 -79 0
-80 81 -82 0
83 84 -85 0
-86 87 -88 0
89 90 -91 0
-92 93 -94 0
95 96 -97 0
-98 99 -100 0
1 -5 10 0
-2 6 -11 0
3 -7 12 0
-4 8 -13 0
5 -9 14 0
-6 10 -15 0
7 -11 16 0
-8 12 -17 0
9 -13 18 0
-10 14 -19 0
11 -15 20 0
-12 16 -21 0
13 -17 22 0
-14 18 -23 0
15 -19 24 0
-16 20 -25 0
17 -21 26 0
-18 22 -27 0
19 -23 28 0
-20 24 -29 0
21 -25 30 0
-22 26 -31 0
23 -27 32 0
-24 28 -33 0
25 -29 34 0
-26 30 -35 0
27 -31 36 0
-28 32 -37 0
29 -33 38 0
-30 34 -39 0
31 -35 40 0
-32 36 -41 0
33 -37 42 0
-34 38 -43 0
35 -39 44 0
-36 40 -45 0
37 -41 46 0
-38 42 -47 0
39 -43 48 0
-40 44 -49 0
41 -45 50 0
-42 46 -51 0
43 -47 52 0
-44 48 -53 0
45 -49 54 0
-46 50 -55 0
47 -51 56 0
-48 52 -57 0
49 -53 58 0
-50 54 -59 0
51 -55 60 0
-52 56 -61 0
53 -57 62 0
-54 58 -63 0
55 -59 64 0
-56 60 -65 0
57 -61 66 0
-58 62 -67 0
59 -63 68 0
-60 64 -69 0
61 -65 70 0
-62 66 -71 0
63 -67 72 0
-64 68 -73 0
65 -69 74 0
-66 70 -75 0
67 -71 76 0
-68 72 -77 0
69 -73 78 0
-70 74 -79 0
71 -75 80 0
-72 76 -81 0
73 -77 82 0
-74 78 -83 0
75 -79 84 0
-76 80 -85 0
77 -81 86 0
-78 82 -87 0
79 -83 88 0
-80 84 -89 0
81 -85 90 0
-82 86 -91 0
83 -87 92 0
-84 88 -93 0
85 -89 94 0
-86 90 -95 0
87 -91 96 0
-88 92 -97 0
89 -93 98 0
-90 94 -99 0
91 -95 100 0
-92 96 -1 0
93 -97 2 0
-94 98 -3 0
95 -99 4 0
-96 100 -5 0
1 20 50 0
2 21 51 0
3 22 52 0
4 23 53 0
5 24 54 0
6 25 55 0
7 26 56 0
8 27 57 0
9 28 58 0
10 29 59 0
11 30 60 0
12 31 61 0
13 32 62 0
14 33 63 0
15 34 64 0
16 35 65 0
17 36 66 0
18 37 67 0
19 38 68 0
20 39 69 0
21 40 70 0
22 41 71 0
23 42 72 0
24 43 73 0
25 44 74 0
26 45 75 0
27 46 76 0
28 47 77 0
29 48 78 0
30 49 79 0
31 50 80 0
32 51 81 0
33 52 82 0
34 53 83 0
35 54 84 0
36 55 85 0
37 56 86 0
38 57 87 0
39 58 88 0
40 59 89 0
-1 -20 -50 0
-2 -21 -51 0
-3 -22 -52 0
-4 -23 -53 0
-5 -24 -54 0
-6 -25 -55 0
-7 -26 -56 0
-8 -27 -57 0
-9 -28 -58 0
-10 -29 -59 0
-11 -30 -60 0
-12 -31 -61 0
-13 -32 -62 0
-14 -33 -63 0
-15 -34 -64 0
-16 -35 -65 0
-17 -36 -66 0
-18 -37 -67 0
-19 -38 -68 0
-20 -39 -69 0
-21 -40 -70 0
-22 -41 -71 0
-23 -42 -72 0
-24 -43 -73 0
-25 -44 -74 0
-26 -45 -75 0
-27 -46 -76 0
-28 -47 -77 0
-29 -48 -78 0
-30 -49 -79 0
-31 -50 -80 0
-32 -51 -81 0
-33 -52 -82 0
-34 -53 -83 0
-35 -54 -84 0
-36 -55 -85 0
-37 -56 -86 0
-38 -57 -87 0
-39 -58 -88 0
-40 -59 -89 0
41 60 90 0
42 61 91 0
43 62 92 0
44 63 93 0
45 64 94 0
46 65 95 0
47 66 96 0
48 67 97 0
49 68 98 0
50 69 99 0
51 70 100 0
52 71 -1 0
53 72 -2 0
54 73 -3 0
55 74 -4 0
56 75 -5 0
57 76 -6 0
58 77 -7 0
59 78 -8 0
60 79 -9 0
61 80 -10 0
62 81 -11 0
63 82 -12 0
64 83 -13 0
65 84 -14 0
66 85 -15 0
67 86 -16 0
68 87 -17 0
69 88 -18 0
70 89 -19 0
71 90 -20 0
72 91 -21 0
73 92 -22 0
74 93 -23 0
75 94 -24 0
76 95 -25 0
77 96 -26 0
78 97 -27 0
79 98 -28 0
80 99 -29 0
81 100 -30 0
-41 -60 -90 0
-42 -61 -91 0
-43 -62 -92 0
-44 -63 -93 0
-45 -64 -94 0
-46 -65 -95 0
-47 -66 -96 0
-48 -67 -97 0
-49 -68 -98 0
-50 -69 -99 0
-51 -70 -100 0
-52 -71 1 0
-53 -72 2 0
-54 -73 3 0
-55 -74 4 0
-56 -75 5 0
-57 -76 6 0
-58 -77 7 0
-59 -78 8 0
-60 -79 9 0
-61 -80 10 0
-62 -81 11 0
-63 -82 12 0
-64 -83 13 0
-65 -84 14 0
-66 -85 15 0
-67 -86 16 0
-68 -87 17 0
-69 -88 18 0
-70 -89 19 0
82 -90 1 0
83 -91 2 0
84 -92 3 0
85 -93 4 0
86 -94 5 0
87 -95 6 0
88 -96 7 0
89 -97 8 0
90 -98 9 0
91 -99 10 0
92 -100 11 0
93 -1 12 0
94 -2 13 0
95 -3 14 0
96 -4 15 0
97 -5 16 0
98 -6 17 0
99 -7 18 0
100 -8 19 0
-82 90 -20 0
-83 91 -21 0
-84 92 -22 0
-85 93 -23 0
-86 94 -24 0
-87 95 -25 0
-88 96 -26 0
-89 97 -27 0
-90 98 -28 0
-91 99 -29 0
-92 100 -30 0
1 2 3 0
4 5 6 0
7 8 9 0
10 11 12 0
13 14 15 0
16 17 18 0
19 20 21 0
22 23 24 0
25 26 27 0
28 29 30 0
]]

-- ============================================================================
-- Test instances: Unsatisfiable
-- ============================================================================

-- Small UNSAT: contradictory unit clauses + implications
local UNSAT_1 = [[
p cnf 4 8
1 2 0
1 -2 0
-1 2 0
-1 -2 0
3 4 0
3 -4 0
-3 4 0
-3 -4 0
]]

-- UNSAT: parity-like constraints
local UNSAT_2 = [[
p cnf 6 18
1 2 3 0
1 -2 -3 0
-1 2 -3 0
-1 -2 3 0
4 5 6 0
4 -5 -6 0
-4 5 -6 0
-4 -5 6 0
-1 -4 0
-2 -5 0
-3 -6 0
1 4 0
2 5 0
3 6 0
1 2 -4 0
-1 -2 4 0
3 -5 6 0
-3 5 -6 0
]]

-- ============================================================================
-- Structured problems: Pigeonhole principle (4 pigeons, 3 holes)
-- PHP(4,3): 4 pigeons must go into 3 holes, no two pigeons in same hole
-- This is classically unsatisfiable.
-- Variables: p_i_j means pigeon i goes to hole j
-- var(i,j) = (i-1)*3 + j for i=1..4, j=1..3  => 12 vars
-- ============================================================================

local PIGEONHOLE_4_3 = [[
p cnf 12 22
c Pigeonhole: 4 pigeons, 3 holes
c Variables: p(i,j) = (i-1)*3+j, pigeon i in hole j
c At-least-one hole per pigeon:
1 2 3 0
4 5 6 0
7 8 9 0
10 11 12 0
c At-most-one pigeon per hole:
-1 -4 0
-1 -7 0
-1 -10 0
-4 -7 0
-4 -10 0
-7 -10 0
-2 -5 0
-2 -8 0
-2 -11 0
-5 -8 0
-5 -11 0
-8 -11 0
-3 -6 0
-3 -9 0
-3 -12 0
-6 -9 0
-6 -12 0
-9 -12 0
]]

-- ============================================================================
-- Structured problems: Graph coloring (3-coloring on K4)
-- K4 has 4 vertices, each pair connected. 3 colors.
-- var(v,c) = (v-1)*3 + c for v=1..4, c=1..3  => 12 vars
-- SAT for K4 with 4 colors, UNSAT for K4 with 2 colors
-- Let's do 3-coloring of a 5-cycle (which IS 3-colorable)
-- ============================================================================

local GRAPH_COLORING_5CYCLE = [[
p cnf 15 35
c 3-coloring of 5-cycle (vertices 1-5)
c var(v,c) = (v-1)*3 + c, v=1..5, c=1..3
c Each vertex has at least one color:
1 2 3 0
4 5 6 0
7 8 9 0
10 11 12 0
13 14 15 0
c Each vertex has at most one color:
-1 -2 0
-1 -3 0
-2 -3 0
-4 -5 0
-4 -6 0
-5 -6 0
-7 -8 0
-7 -9 0
-8 -9 0
-10 -11 0
-10 -12 0
-11 -12 0
-13 -14 0
-13 -15 0
-14 -15 0
c Adjacent vertices have different colors:
c Edge 1-2:
-1 -4 0
-2 -5 0
-3 -6 0
c Edge 2-3:
-4 -7 0
-5 -8 0
-6 -9 0
c Edge 3-4:
-7 -10 0
-8 -11 0
-9 -12 0
c Edge 4-5:
-10 -13 0
-11 -14 0
-12 -15 0
c Edge 5-1:
-13 -1 0
-14 -2 0
-15 -3 0
]]

-- ============================================================================
-- Run a single SAT instance and return result + stats
-- ============================================================================

function run_instance(name, dimacs_text, expected_result)
    local num_vars, clauses = parse_dimacs(dimacs_text)
    local s = solver_new(num_vars, clauses)
    local result, assignment = solver_solve(s)

    local checksum = 0
    if result == "SAT" and assignment then
        -- Verify
        local ok, bad_clause = verify_sat(num_vars, clauses, assignment)
        if not ok then
            error(name .. ": SAT verification failed at clause " .. bad_clause)
        end
        checksum = assignment_checksum(assignment, num_vars)
    end

    if expected_result and result ~= expected_result then
        error(name .. ": expected " .. expected_result .. " but got " .. result)
    end

    return result, checksum, s.conflicts, s.decisions, s.propagations
end

-- ============================================================================
-- Run generated random 3-SAT instances
-- ============================================================================

function run_random_instances()
    local total_checksum = 0

    -- Generate several random 3-SAT instances near phase transition
    -- ratio ~4.26, use 20 vars => ~85 clauses
    for trial = 1, 5 do
        local nv = 20
        local nc = math_floor(nv * 4.26)
        local clauses = generate_random_3sat(nv, nc)
        local s = solver_new(nv, clauses)
        local result, assignment = solver_solve(s)

        if result == "SAT" and assignment then
            local ok, bad = verify_sat(nv, clauses, assignment)
            if not ok then
                error("Random instance " .. trial .. ": verification failed at clause " .. bad)
            end
            total_checksum = (total_checksum + assignment_checksum(assignment, nv)) % 1000000007
        else
            -- UNSAT is valid for random instances
            total_checksum = (total_checksum + trial * 999983) % 1000000007
        end
    end

    -- Larger random instances: 40 vars, ~170 clauses
    for trial = 1, 3 do
        local nv = 40
        local nc = math_floor(nv * 4.26)
        local clauses = generate_random_3sat(nv, nc)
        local s = solver_new(nv, clauses)
        local result, assignment = solver_solve(s)

        if result == "SAT" and assignment then
            local ok, bad = verify_sat(nv, clauses, assignment)
            if not ok then
                error("Random large instance " .. trial .. ": verification failed at clause " .. bad)
            end
            total_checksum = (total_checksum + assignment_checksum(assignment, nv)) % 1000000007
        else
            total_checksum = (total_checksum + trial * 999979) % 1000000007
        end
    end

    return total_checksum
end

-- ============================================================================
-- Full benchmark iteration
-- ============================================================================

function run_one_iteration()
    local total_checksum = 0
    local instance_count = 0

    -- Trivially satisfiable
    local r, cs
    r, cs = run_instance("trivial_sat_1", TRIVIAL_SAT_1, "SAT")
    total_checksum = (total_checksum + cs) % 1000000007
    instance_count = instance_count + 1

    r, cs = run_instance("trivial_sat_2", TRIVIAL_SAT_2, "SAT")
    total_checksum = (total_checksum + cs) % 1000000007
    instance_count = instance_count + 1

    r, cs = run_instance("trivial_sat_3", TRIVIAL_SAT_3, "SAT")
    total_checksum = (total_checksum + cs) % 1000000007
    instance_count = instance_count + 1

    -- Challenging satisfiable
    r, cs = run_instance("challenging_sat_1", CHALLENGING_SAT_1, "SAT")
    total_checksum = (total_checksum + cs) % 1000000007
    instance_count = instance_count + 1

    r, cs = run_instance("challenging_sat_2", CHALLENGING_SAT_2, "SAT")
    total_checksum = (total_checksum + cs) % 1000000007
    instance_count = instance_count + 1

    r, cs = run_instance("challenging_sat_3", CHALLENGING_SAT_3, "SAT")
    total_checksum = (total_checksum + cs) % 1000000007
    instance_count = instance_count + 1

    -- Unsatisfiable
    r, cs = run_instance("unsat_1", UNSAT_1, "UNSAT")
    instance_count = instance_count + 1

    r, cs = run_instance("unsat_2", UNSAT_2, "UNSAT")
    instance_count = instance_count + 1

    -- Structured: Pigeonhole (UNSAT)
    r, cs = run_instance("pigeonhole_4_3", PIGEONHOLE_4_3, "UNSAT")
    instance_count = instance_count + 1

    -- Structured: Graph coloring (SAT)
    r, cs = run_instance("graph_coloring_5cycle", GRAPH_COLORING_5CYCLE, "SAT")
    total_checksum = (total_checksum + cs) % 1000000007
    instance_count = instance_count + 1

    -- Random instances
    prng_reset()
    local rand_cs = run_random_instances()
    total_checksum = (total_checksum + rand_cs) % 1000000007
    instance_count = instance_count + 8

    return total_checksum, instance_count
end

-- ============================================================================
-- Main: loop until target time reached
-- ============================================================================

function main()
    for i = 1, 20 do
        local cs, count = run_one_iteration()
        if cs ~= 656674380 then
            error("Wrong checksum " .. cs)
        end
        if count ~= 18 then
            error("Wrong number of iterations " .. count)
        end
    end
end

main()

end

bench.runCode(test, "sat")
