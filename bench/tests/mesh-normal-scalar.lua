--!strict
local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

function test()

    type Vertex = {
        pX: number, pY: number, pZ: number,
        uvX: number, uvY: number, uvZ: number,
        nX: number, nY: number, nZ: number,
        tX: number, tY: number, tZ: number,
        bX: number, bY: number, bZ: number,
        h: number
    }

    local grid_size = 100

    local mesh: {
        vertices: {Vertex},
        indices: {number},
        triangle_cone_p: {{x: number, y: number, z: number}},
        triangle_cone_n: {{x: number, y: number, z: number}}
    } = {
        vertices = table.create(grid_size * grid_size),
        indices = table.create((grid_size - 1) * (grid_size - 1) * 6),
        triangle_cone_p = table.create((grid_size - 1) * (grid_size - 1) * 2),
        triangle_cone_n = table.create((grid_size - 1) * (grid_size - 1) * 2)
    }

    local function init_vertices()
        local i = 1
        for y = 1,grid_size do
            for x = 1,grid_size do
                local v: Vertex = {}

                v.pX = x
                v.pY = y
                v.pZ = math.cos(x) + math.sin(y)

                v.uvX = (x-1)/(grid_size-1)
                v.uvY = (y-1)/(grid_size-1)
                v.uvZ = 0

                v.nX = 0
                v.nY = 0
                v.nZ = 0

                v.bX = 0
                v.bY = 0
                v.bZ = 0

                v.tX = 0
                v.tY = 0
                v.tZ = 0

                v.h = 0

                mesh.vertices[i] = v
                i += 1
            end
        end
    end

    local function init_indices()
        local i = 1
        for y = 1,grid_size-1 do
            for x = 1,grid_size-1 do
                mesh.indices[i] = x + (y-1)*grid_size
                i += 1
                mesh.indices[i] = x + y*grid_size
                i += 1
                mesh.indices[i] = (x+1) + (y-1)*grid_size
                i += 1
                mesh.indices[i] = (x+1) + (y-1)*grid_size
                i += 1
                mesh.indices[i] = x + y*grid_size
                i += 1
                mesh.indices[i] = (x+1) + y*grid_size
                i += 1
            end
        end
    end

    local function calculate_normals()
        local norm_sum = 0

        for i = 1,#mesh.indices,3 do
            local a = mesh.vertices[mesh.indices[i]]
            local b = mesh.vertices[mesh.indices[i + 1]]
            local c = mesh.vertices[mesh.indices[i + 2]]

            local abx = a.pX - b.pX
            local aby = a.pY - b.pY
            local abz = a.pZ - b.pZ

            local acx = a.pX - c.pX
            local acy = a.pY - c.pY
            local acz = a.pZ - c.pZ

            local nx = aby * acz - abz * acy;
            local ny = abz * acx - abx * acz;
            local nz = abx * acy - aby * acx;

            a.nX += nx
            a.nY += ny
            a.nZ += nz

            b.nX += nx
            b.nY += ny
            b.nZ += nz

            c.nX += nx
            c.nY += ny
            c.nZ += nz
        end

        for _,v in mesh.vertices do
            local magnitude = math.sqrt(v.nX * v.nX + v.nY * v.nY + v.nZ * v.nZ)

            v.nX /= magnitude
            v.nY /= magnitude
            v.nZ /= magnitude

            norm_sum += v.nX * v.nX + v.nY * v.nY + v.nZ * v.nZ
        end

        return norm_sum
    end

    local function compute_triangle_cones()
        local mesh_area = 0

        local pos = 1

        for i = 1,#mesh.indices,3 do
            local p0 = mesh.vertices[mesh.indices[i]]
            local p1 = mesh.vertices[mesh.indices[i + 1]]
            local p2 = mesh.vertices[mesh.indices[i + 2]]

            local p10x = p1.pX - p0.pX
            local p10y = p1.pY - p0.pY
            local p10z = p1.pZ - p0.pZ
            local p20x = p2.pX - p0.pX
            local p20y = p2.pY - p0.pY
            local p20z = p2.pZ - p0.pZ

            local normalx = p10y * p20z - p10z * p20y;
            local normaly = p10z * p20x - p10x * p20z;
            local normalz = p10x * p20y - p10y * p20x;

            local area = math.sqrt(normalx * normalx + normaly * normaly + normalz * normalz)
            local invarea = if area == 0 then 0 else 1 / area;

            local rx = (p0.pX + p1.pX + p2.pX) / 3
            local ry = (p0.pY + p1.pY + p2.pY) / 3
            local rz = (p0.pZ + p1.pZ + p2.pZ) / 3

            mesh.triangle_cone_p[pos] = { x = rx, y = ry, z = rz }
            mesh.triangle_cone_n[pos] = { x = normalx * invarea, y = normaly * invarea, z = normalz * invarea}
            pos += 1

            mesh_area += area
        end

        return mesh_area
    end

    local function compute_tangent_space()
        local checksum = 0

        for i = 1,#mesh.indices,3 do
            local a = mesh.vertices[mesh.indices[i]]
            local b = mesh.vertices[mesh.indices[i + 1]]
            local c = mesh.vertices[mesh.indices[i + 2]]

            local x1 = b.pX - a.pX
            local x2 = c.pX - a.pX
            local y1 = b.pY - a.pY
            local y2 = c.pY - a.pY
            local z1 = b.pZ - a.pZ
            local z2 = c.pZ - a.pZ

            local s1 = b.uvX - a.uvX
            local s2 = c.uvX - a.uvX
            local t1 = b.uvY - a.uvY
            local t2 = c.uvY - a.uvY

            local r = 1.0 / (s1 * t2 - s2 * t1);
            local sdirX = (t2 * x1 - t1 * x2) * r
            local sdirY = (t2 * y1 - t1 * y2) * r
            local sdirZ = (t2 * z1 - t1 * z2) * r
            local tdirX = (s1 * x2 - s2 * x1) * r
            local tdirY = (s1 * y2 - s2 * y1) * r
            local tdirZ = (s1 * z2 - s2 * z1) * r

            a.tX += sdirX
            a.tY += sdirY
            a.tZ += sdirZ
            b.tX += sdirX
            b.tY += sdirY
            b.tZ += sdirZ
            c.tX += sdirX
            c.tY += sdirY
            c.tZ += sdirZ

            a.bX += tdirX
            a.bY += tdirY
            a.bZ += tdirZ
            b.bX += tdirX
            b.bY += tdirY
            b.bZ += tdirZ
            c.bX += tdirX
            c.bY += tdirY
            c.bZ += tdirZ
        end

        for _,v in mesh.vertices do
            local tX = v.tX
            local tY = v.tY
            local tZ = v.tZ

            -- Gram-Schmidt orthogonalize
            local ndt = v.nX * tX + v.nY * tY + v.nZ * tZ
            local tmnsX = tX - v.nX * ndt
            local tmnsY = tY - v.nY * ndt
            local tmnsZ = tZ - v.nZ * ndt
            local l = math.sqrt(tmnsX * tmnsX + tmnsY * tmnsY + tmnsZ * tmnsZ)

            local invl = 1 / l
            v.tX = tmnsX * invl
            v.tY = tmnsY * invl
            v.tZ = tmnsZ * invl

            local normalx = v.nY * tZ - v.nZ * tY;
            local normaly = v.nZ * tX - v.nX * tZ;
            local normalz = v.nX * tY - v.nY * tX;

            local ht = normalx * v.bX + normaly * v.bY + normalz * v.bZ

            v.h = ht < 0 and -1 or 1

            checksum += v.tX + v.h
        end

        return checksum
    end

    init_vertices()
    init_indices()
    calculate_normals()
    compute_triangle_cones()
    compute_tangent_space()
end

bench.runCode(test, "mesh-normal-scalar")
