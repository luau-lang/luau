local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

    --The Computer Language Benchmarks Game
    -- https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
    --contributed by Mike Pall

    local PI = 3.141592653589793
    local SOLAR_MASS = 4 * PI * PI
    local DAYS_PER_YEAR = 365.24

    type Body = { pos: vector, vel: vector, mass: number }
    
    local bodies: {Body} = {
        { --Sun
            pos = vector.create(0, 0, 0),
            vel = vector.create(0, 0, 0),
            mass = SOLAR_MASS
        },
        { --Jupiter
            pos = vector.create(4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01),
            vel = vector.create(1.66007664274403694e-03 * DAYS_PER_YEAR, 7.69901118419740425e-03 * DAYS_PER_YEAR, -6.90460016972063023e-05 * DAYS_PER_YEAR),
            mass = 9.54791938424326609e-04 * SOLAR_MASS
        },
        { --Saturn
            pos = vector.create(8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01),
            vel = vector.create(-2.76742510726862411e-03 * DAYS_PER_YEAR, 4.99852801234917238e-03 * DAYS_PER_YEAR, 2.30417297573763929e-05 * DAYS_PER_YEAR),
            mass = 2.85885980666130812e-04 * SOLAR_MASS
        },
        { --Uranus
            pos = vector.create(1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01),
            vel = vector.create(2.96460137564761618e-03 * DAYS_PER_YEAR, 2.37847173959480950e-03 * DAYS_PER_YEAR, -2.96589568540237556e-05 * DAYS_PER_YEAR),
            mass = 4.36624404335156298e-05 * SOLAR_MASS
        },
        { --Neptune
            pos = vector.create(1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01),
            vel = vector.create(2.68067772490389322e-03 * DAYS_PER_YEAR, 1.62824170038242295e-03 * DAYS_PER_YEAR, -9.51592254519715870e-05 * DAYS_PER_YEAR),
            mass = 5.15138902046611451e-05 * SOLAR_MASS
        }
    }

    local function advance(bodies: {Body}, nbody: number, dt: number)
        for i = 1, nbody do
            local bi = bodies[i]
            local bipos, bimass = bi.pos, bi.mass
            local bivel = bi.vel

            for j = i + 1, nbody do
                local bj = bodies[j]

                local dpos = bipos - bj.pos
                local distance = vector.magnitude(dpos)

                local mag = dt / (distance * distance * distance)
                local bim, bjm = bimass * mag, bj.mass * mag

                bivel -= dpos * bjm
                bj.vel += dpos * bim
            end
            
            bi.vel = bivel
        end

        for i = 1, nbody do
            local bi = bodies[i]
            bi.pos += dt * bi.vel
        end
    end

    local function offsetMomentum(bodies: {Body}, nbody: number)
        local p = vector.create(0, 0, 0)

        for i = 1, nbody do
            local bi = bodies[i]
            p += bi.vel * bi.mass
        end

        bodies[1].vel = -p / SOLAR_MASS
    end

    local N = 20000
    local nbody = #bodies

    local ts0 = os.clock()
    offsetMomentum(bodies, nbody)
    for i = 1, N do advance(bodies, nbody, 0.01) end
    local ts1 = os.clock()

    return ts1 - ts0
end

bench.runCode(test, "n-body-vec")