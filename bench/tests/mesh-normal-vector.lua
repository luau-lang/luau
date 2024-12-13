--!strict
local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

function test()

    type Vertex = { p: vector, uv: vector, n: vector, t: vector, b: vector, h: number }

    local grid_size = 100

    local mesh: {
        vertices: {Vertex},
        indices: {number},
        triangle_cone_p: {vector},
        triangle_cone_n: {vector}
    } = {
        vertices = table.create(grid_size * grid_size),
        indices = table.create((grid_size - 1) * (grid_size - 1) * 6),
        triangle_cone_p = table.create((grid_size - 1) * (grid_size - 1) * 2),
        triangle_cone_n = table.create((grid_size - 1) * (grid_size - 1) * 2)
    }
    
    function init_vertices()
        local i = 1
        for y = 1,grid_size do
            for x = 1,grid_size do
                local v: Vertex = {}
                
                v.p = vector.create(x, y, math.cos(x) + math.sin(y))
                v.uv = vector.create((x-1)/(grid_size-1), (y-1)/(grid_size-1), 0)
                v.n = vector.create(0, 0, 0)
                v.b = vector.create(0, 0, 0)
                v.t = vector.create(0, 0, 0)
                v.h = 0
                
                mesh.vertices[i] = v
                i += 1
            end
        end
    end
    
    function init_indices()
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
    
    function calculate_normals()
        local norm_sum = 0
        
        for i = 1,#mesh.indices,3 do
            local a = mesh.vertices[mesh.indices[i]]
            local b = mesh.vertices[mesh.indices[i + 1]]
            local c = mesh.vertices[mesh.indices[i + 2]]
            
            local n = vector.cross(a.p - b.p, a.p - c.p)
            
            a.n += n
            b.n += n
            c.n += n
        end
        
        for _,v in ipairs(mesh.vertices) do
            v.n = vector.normalize(v.n)
    
            norm_sum += vector.dot(v.n, v.n)
        end
        
        return norm_sum
    end
    
    function compute_triangle_cones()
        local mesh_area = 0
        
        local pos = 1

        for i = 1,#mesh.indices,3 do
            local p0 = mesh.vertices[mesh.indices[i]]
            local p1 = mesh.vertices[mesh.indices[i + 1]]
            local p2 = mesh.vertices[mesh.indices[i + 2]]
            
            local p10 = p1.p - p0.p
            local p20 = p2.p - p0.p
            
            local normal = vector.cross(p10, p20)
    
            local area = vector.magnitude(normal)
            local invarea = (area == 0) and 0 or 1 / area;
            
            mesh.triangle_cone_p[pos] = (p0.p + p1.p + p2.p) / 3
            mesh.triangle_cone_n[pos] = normal * invarea
            pos += 1
    
            mesh_area += area
        end
    
        return mesh_area
    end
    
    function compute_tangent_space()
        local checksum = 0
        
        for i = 1,#mesh.indices,3 do
            local a = mesh.vertices[mesh.indices[i]]
            local b = mesh.vertices[mesh.indices[i + 1]]
            local c = mesh.vertices[mesh.indices[i + 2]]
    
            local vba = b.p - a.p
            local vca = c.p - a.p
    
            local uvba = b.uv - a.uv
            local uvca = c.uv - a.uv
    
            local r = 1.0 / (uvba.X * uvca.Y - uvca.X * uvba.Y);
    
            local sdir = (uvca.Y * vba - uvba.Y * vca) * r
            local tdir = (uvba.X * vca - uvca.X * vba) * r
            
            a.t += sdir
            b.t += sdir
            c.t += sdir
            
            a.b += tdir
            b.b += tdir
            c.b += tdir
        end
        
        for _,v in ipairs(mesh.vertices) do
            local t = v.t
            
            -- Gram-Schmidt orthogonalize
            v.t = vector.normalize(t - v.n * vector.dot(v.n, t))
            
            local ht = vector.dot(vector.cross(v.n, t), v.b)
            
            v.h = ht < 0 and -1 or 1
            
            checksum += v.t.X + v.h
        end
        
        return checksum
    end
    

    init_vertices()
    init_indices()
    calculate_normals()
    compute_triangle_cones()
    compute_tangent_space()
end

bench.runCode(test, "mesh-normal-vector")
