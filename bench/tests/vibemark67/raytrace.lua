local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

-- 3D Ray Tracer Benchmark
-- A recursive ray tracer with BVH acceleration, Phong shading, reflections,
-- refraction, shadow rays, and multiple scene configurations.
-- Style: vectors as plain {x,y,z} tables, global functions, math-heavy.

local math_sqrt = math.sqrt
local math_abs = math.abs
local math_min = math.min
local math_max = math.max
local math_floor = math.floor
local math_huge = math.huge
local math_pi = math.pi
local math_sin = math.sin
local math_cos = math.cos
local math_tan = math.tan

-- ============================================================================
-- Vector3 operations (plain tables, no metatables)
-- ============================================================================

function vec3(x, y, z)
    return {x = x, y = y, z = z}
end

function vec3_add(a, b)
    return {x = a.x + b.x, y = a.y + b.y, z = a.z + b.z}
end

function vec3_sub(a, b)
    return {x = a.x - b.x, y = a.y - b.y, z = a.z - b.z}
end

function vec3_mul(v, s)
    return {x = v.x * s, y = v.y * s, z = v.z * s}
end

function vec3_div(v, s)
    return {x = v.x / s, y = v.y / s, z = v.z / s}
end

function vec3_mul_vec(a, b)
    return {x = a.x * b.x, y = a.y * b.y, z = a.z * b.z}
end

function vec3_dot(a, b)
    return a.x * b.x + a.y * b.y + a.z * b.z
end

function vec3_cross(a, b)
    return {
        x = a.y * b.z - a.z * b.y,
        y = a.z * b.x - a.x * b.z,
        z = a.x * b.y - a.y * b.x
    }
end

function vec3_length(v)
    return math_sqrt(v.x * v.x + v.y * v.y + v.z * v.z)
end

function vec3_length_sq(v)
    return v.x * v.x + v.y * v.y + v.z * v.z
end

function vec3_normalize(v)
    local len = math_sqrt(v.x * v.x + v.y * v.y + v.z * v.z)
    if len < 1e-10 then return {x = 0, y = 0, z = 0} end
    local inv = 1.0 / len
    return {x = v.x * inv, y = v.y * inv, z = v.z * inv}
end

function vec3_negate(v)
    return {x = -v.x, y = -v.y, z = -v.z}
end

function vec3_reflect(v, n)
    local d = 2.0 * vec3_dot(v, n)
    return {x = v.x - d * n.x, y = v.y - d * n.y, z = v.z - d * n.z}
end

function vec3_lerp(a, b, t)
    return {
        x = a.x + (b.x - a.x) * t,
        y = a.y + (b.y - a.y) * t,
        z = a.z + (b.z - a.z) * t
    }
end

function vec3_min(a, b)
    return {
        x = math_min(a.x, b.x),
        y = math_min(a.y, b.y),
        z = math_min(a.z, b.z)
    }
end

function vec3_max(a, b)
    return {
        x = math_max(a.x, b.x),
        y = math_max(a.y, b.y),
        z = math_max(a.z, b.z)
    }
end

function vec3_distance(a, b)
    local dx = b.x - a.x
    local dy = b.y - a.y
    local dz = b.z - a.z
    return math_sqrt(dx * dx + dy * dy + dz * dz)
end

function vec3_clamp(v, lo, hi)
    return {
        x = math_max(lo, math_min(hi, v.x)),
        y = math_max(lo, math_min(hi, v.y)),
        z = math_max(lo, math_min(hi, v.z))
    }
end

-- ============================================================================
-- Color operations
-- ============================================================================

function color_new(r, g, b)
    return {r = r, g = g, b = b}
end

function color_add(a, b)
    return {r = a.r + b.r, g = a.g + b.g, b = a.b + b.b}
end

function color_mul(c, s)
    return {r = c.r * s, g = c.g * s, b = c.b * s}
end

function color_mul_color(a, b)
    return {r = a.r * b.r, g = a.g * b.g, b = a.b * b.b}
end

function color_clamp(c)
    return {
        r = math_max(0, math_min(1, c.r)),
        g = math_max(0, math_min(1, c.g)),
        b = math_max(0, math_min(1, c.b))
    }
end

-- ============================================================================
-- Ray
-- ============================================================================

function ray_new(origin, direction)
    return {origin = origin, direction = direction}
end

function ray_point_at(ray, t)
    return {
        x = ray.origin.x + ray.direction.x * t,
        y = ray.origin.y + ray.direction.y * t,
        z = ray.origin.z + ray.direction.z * t
    }
end

-- ============================================================================
-- Materials
-- ============================================================================

function material_new(color, specular, reflectivity, transparency, ior, shininess)
    return {
        color = color or {r = 0.5, g = 0.5, b = 0.5},
        specular = specular or 0.0,
        reflectivity = reflectivity or 0.0,
        transparency = transparency or 0.0,
        ior = ior or 1.5,
        shininess = shininess or 32
    }
end

function material_diffuse(r, g, b)
    return material_new(color_new(r, g, b), 0.3, 0.0, 0.0, 1.5, 32)
end

function material_reflective(r, g, b, refl)
    return material_new(color_new(r, g, b), 0.8, refl or 0.8, 0.0, 1.5, 64)
end

function material_glass(r, g, b, ior)
    return material_new(color_new(r, g, b), 0.9, 0.1, 0.9, ior or 1.5, 128)
end

-- ============================================================================
-- AABB (Axis-Aligned Bounding Box)
-- ============================================================================

function aabb_new(min_pt, max_pt)
    return {min = min_pt, max = max_pt}
end

function aabb_expand(box, point)
    return {
        min = vec3_min(box.min, point),
        max = vec3_max(box.max, point)
    }
end

function aabb_union(a, b)
    return {
        min = vec3_min(a.min, b.min),
        max = vec3_max(a.max, b.max)
    }
end

function aabb_centroid(box)
    return {
        x = (box.min.x + box.max.x) * 0.5,
        y = (box.min.y + box.max.y) * 0.5,
        z = (box.min.z + box.max.z) * 0.5
    }
end

function aabb_surface_area(box)
    local dx = box.max.x - box.min.x
    local dy = box.max.y - box.min.y
    local dz = box.max.z - box.min.z
    return 2.0 * (dx * dy + dy * dz + dz * dx)
end

function aabb_longest_axis(box)
    local dx = box.max.x - box.min.x
    local dy = box.max.y - box.min.y
    local dz = box.max.z - box.min.z
    if dx >= dy and dx >= dz then return 1 end
    if dy >= dz then return 2 end
    return 3
end

-- Slab method ray-AABB intersection
function aabb_intersect(box, ray_origin, ray_dir_inv, tmin_limit, tmax_limit)
    local tx1 = (box.min.x - ray_origin.x) * ray_dir_inv.x
    local tx2 = (box.max.x - ray_origin.x) * ray_dir_inv.x
    local tmin = math_min(tx1, tx2)
    local tmax = math_max(tx1, tx2)

    local ty1 = (box.min.y - ray_origin.y) * ray_dir_inv.y
    local ty2 = (box.max.y - ray_origin.y) * ray_dir_inv.y
    tmin = math_max(tmin, math_min(ty1, ty2))
    tmax = math_min(tmax, math_max(ty1, ty2))

    local tz1 = (box.min.z - ray_origin.z) * ray_dir_inv.z
    local tz2 = (box.max.z - ray_origin.z) * ray_dir_inv.z
    tmin = math_max(tmin, math_min(tz1, tz2))
    tmax = math_min(tmax, math_max(tz1, tz2))

    if tmax < math_max(tmin, tmin_limit) then return false end
    if tmin > tmax_limit then return false end
    return true
end

-- ============================================================================
-- Sphere intersection
-- ============================================================================

function sphere_new(center, radius, mat)
    local r = radius
    local bbox = aabb_new(
        {x = center.x - r, y = center.y - r, z = center.z - r},
        {x = center.x + r, y = center.y + r, z = center.z + r}
    )
    return {
        type = "sphere",
        center = center,
        radius = radius,
        radius_sq = radius * radius,
        material = mat,
        bounds = bbox
    }
end

function sphere_intersect(sphere, ray, t_min, t_max)
    local oc_x = ray.origin.x - sphere.center.x
    local oc_y = ray.origin.y - sphere.center.y
    local oc_z = ray.origin.z - sphere.center.z
    local dir = ray.direction

    local a = dir.x * dir.x + dir.y * dir.y + dir.z * dir.z
    local half_b = oc_x * dir.x + oc_y * dir.y + oc_z * dir.z
    local c = oc_x * oc_x + oc_y * oc_y + oc_z * oc_z - sphere.radius_sq

    local discriminant = half_b * half_b - a * c
    if discriminant < 0 then return nil end

    local sqrt_disc = math_sqrt(discriminant)
    local inv_a = 1.0 / a
    local t = (-half_b - sqrt_disc) * inv_a
    if t < t_min or t > t_max then
        t = (-half_b + sqrt_disc) * inv_a
        if t < t_min or t > t_max then return nil end
    end

    local px = ray.origin.x + dir.x * t
    local py = ray.origin.y + dir.y * t
    local pz = ray.origin.z + dir.z * t
    local inv_r = 1.0 / sphere.radius
    local nx = (px - sphere.center.x) * inv_r
    local ny = (py - sphere.center.y) * inv_r
    local nz = (pz - sphere.center.z) * inv_r

    return {
        t = t,
        point = {x = px, y = py, z = pz},
        normal = {x = nx, y = ny, z = nz},
        material = sphere.material
    }
end

-- ============================================================================
-- Plane intersection
-- ============================================================================

function plane_new(point, normal, mat)
    return {
        type = "plane",
        point = point,
        normal = vec3_normalize(normal),
        material = mat,
        bounds = nil -- planes have infinite extent, not in BVH
    }
end

function plane_intersect(pl, ray, t_min, t_max)
    local denom = vec3_dot(pl.normal, ray.direction)
    if math_abs(denom) < 1e-8 then return nil end

    local diff = vec3_sub(pl.point, ray.origin)
    local t = vec3_dot(diff, pl.normal) / denom
    if t < t_min or t > t_max then return nil end

    local point = ray_point_at(ray, t)
    local normal = pl.normal
    -- Make sure normal faces the ray
    if denom > 0 then
        normal = vec3_negate(normal)
    end

    return {
        t = t,
        point = point,
        normal = normal,
        material = pl.material
    }
end

-- ============================================================================
-- Triangle intersection (Moller-Trumbore algorithm)
-- ============================================================================

function triangle_new(v0, v1, v2, mat)
    local edge1 = vec3_sub(v1, v0)
    local edge2 = vec3_sub(v2, v0)
    local normal = vec3_normalize(vec3_cross(edge1, edge2))

    local min_pt = vec3_min(vec3_min(v0, v1), v2)
    local max_pt = vec3_max(vec3_max(v0, v1), v2)
    -- Slightly expand thin bounding boxes
    local eps = 0.0001
    if max_pt.x - min_pt.x < eps then max_pt.x = max_pt.x + eps; min_pt.x = min_pt.x - eps end
    if max_pt.y - min_pt.y < eps then max_pt.y = max_pt.y + eps; min_pt.y = min_pt.y - eps end
    if max_pt.z - min_pt.z < eps then max_pt.z = max_pt.z + eps; min_pt.z = min_pt.z - eps end

    return {
        type = "triangle",
        v0 = v0,
        v1 = v1,
        v2 = v2,
        edge1 = edge1,
        edge2 = edge2,
        normal = normal,
        material = mat,
        bounds = aabb_new(min_pt, max_pt)
    }
end

function triangle_intersect(tri, ray, t_min, t_max)
    local h = vec3_cross(ray.direction, tri.edge2)
    local a = vec3_dot(tri.edge1, h)
    if a > -1e-8 and a < 1e-8 then return nil end

    local f = 1.0 / a
    local s = vec3_sub(ray.origin, tri.v0)
    local u = f * vec3_dot(s, h)
    if u < 0.0 or u > 1.0 then return nil end

    local q = vec3_cross(s, tri.edge1)
    local v = f * vec3_dot(ray.direction, q)
    if v < 0.0 or u + v > 1.0 then return nil end

    local t = f * vec3_dot(tri.edge2, q)
    if t < t_min or t > t_max then return nil end

    local point = ray_point_at(ray, t)
    local normal = tri.normal
    -- Make sure normal faces the ray
    if vec3_dot(normal, ray.direction) > 0 then
        normal = vec3_negate(normal)
    end

    return {
        t = t,
        point = point,
        normal = normal,
        material = tri.material
    }
end

-- ============================================================================
-- Box (Axis-aligned box made of 12 triangles)
-- ============================================================================

function box_new(min_pt, max_pt, mat)
    local triangles = {}
    local x0 = min_pt.x; local y0 = min_pt.y; local z0 = min_pt.z
    local x1 = max_pt.x; local y1 = max_pt.y; local z1 = max_pt.z

    -- Vertices
    local v000 = vec3(x0, y0, z0)
    local v100 = vec3(x1, y0, z0)
    local v010 = vec3(x0, y1, z0)
    local v110 = vec3(x1, y1, z0)
    local v001 = vec3(x0, y0, z1)
    local v101 = vec3(x1, y0, z1)
    local v011 = vec3(x0, y1, z1)
    local v111 = vec3(x1, y1, z1)

    -- Front face (z = z1)
    triangles[#triangles + 1] = triangle_new(v001, v101, v111, mat)
    triangles[#triangles + 1] = triangle_new(v001, v111, v011, mat)
    -- Back face (z = z0)
    triangles[#triangles + 1] = triangle_new(v100, v000, v010, mat)
    triangles[#triangles + 1] = triangle_new(v100, v010, v110, mat)
    -- Top face (y = y1)
    triangles[#triangles + 1] = triangle_new(v010, v011, v111, mat)
    triangles[#triangles + 1] = triangle_new(v010, v111, v110, mat)
    -- Bottom face (y = y0)
    triangles[#triangles + 1] = triangle_new(v000, v100, v101, mat)
    triangles[#triangles + 1] = triangle_new(v000, v101, v001, mat)
    -- Right face (x = x1)
    triangles[#triangles + 1] = triangle_new(v100, v110, v111, mat)
    triangles[#triangles + 1] = triangle_new(v100, v111, v101, mat)
    -- Left face (x = x0)
    triangles[#triangles + 1] = triangle_new(v000, v001, v011, mat)
    triangles[#triangles + 1] = triangle_new(v000, v011, v010, mat)

    return triangles
end

-- ============================================================================
-- Stable merge sort (deterministic across runtimes unlike table.sort)
-- ============================================================================

function stable_sort(arr, compare)
    local n = #arr
    if n <= 1 then return end
    local mid = math_floor(n / 2)
    local left = {}
    local right = {}
    for i = 1, mid do left[i] = arr[i] end
    for i = mid + 1, n do right[i - mid] = arr[i] end
    stable_sort(left, compare)
    stable_sort(right, compare)
    local i, j, k = 1, 1, 1
    local ln, rn = #left, #right
    while i <= ln and j <= rn do
        if not compare(right[j], left[i]) then
            arr[k] = left[i]
            i = i + 1
        else
            arr[k] = right[j]
            j = j + 1
        end
        k = k + 1
    end
    while i <= ln do arr[k] = left[i]; i = i + 1; k = k + 1 end
    while j <= rn do arr[k] = right[j]; j = j + 1; k = k + 1 end
end

-- ============================================================================
-- BVH (Bounding Volume Hierarchy)
-- ============================================================================

function bvh_build(objects)
    if #objects == 0 then
        return nil
    end

    if #objects == 1 then
        return {
            bounds = objects[1].bounds,
            object = objects[1],
            left = nil,
            right = nil
        }
    end

    if #objects == 2 then
        local combined = aabb_union(objects[1].bounds, objects[2].bounds)
        return {
            bounds = combined,
            object = nil,
            left = {bounds = objects[1].bounds, object = objects[1], left = nil, right = nil},
            right = {bounds = objects[2].bounds, object = objects[2], left = nil, right = nil}
        }
    end

    -- Compute combined bounding box
    local combined = objects[1].bounds
    for i = 2, #objects do
        combined = aabb_union(combined, objects[i].bounds)
    end

    -- Find longest axis
    local axis = aabb_longest_axis(combined)

    -- Sort on that axis (stable sort for determinism across runtimes)
    if axis == 1 then
        stable_sort(objects, function(a, b)
            return aabb_centroid(a.bounds).x < aabb_centroid(b.bounds).x
        end)
    elseif axis == 2 then
        stable_sort(objects, function(a, b)
            return aabb_centroid(a.bounds).y < aabb_centroid(b.bounds).y
        end)
    else
        stable_sort(objects, function(a, b)
            return aabb_centroid(a.bounds).z < aabb_centroid(b.bounds).z
        end)
    end

    -- Split at median
    local mid = math_floor(#objects / 2)
    local left_objects = {}
    local right_objects = {}
    for i = 1, mid do
        left_objects[#left_objects + 1] = objects[i]
    end
    for i = mid + 1, #objects do
        right_objects[#right_objects + 1] = objects[i]
    end

    local left_node = bvh_build(left_objects)
    local right_node = bvh_build(right_objects)

    return {
        bounds = combined,
        object = nil,
        left = left_node,
        right = right_node
    }
end

-- Stack-based BVH traversal
function bvh_intersect(node, ray, t_min, t_max)
    if node == nil then return nil end

    local dir = ray.direction
    local dir_inv = {
        x = 1.0 / (math_abs(dir.x) > 1e-10 and dir.x or 1e-10),
        y = 1.0 / (math_abs(dir.y) > 1e-10 and dir.y or 1e-10),
        z = 1.0 / (math_abs(dir.z) > 1e-10 and dir.z or 1e-10)
    }

    local stack = {}
    local stack_top = 1
    stack[1] = node
    local closest_hit = nil
    local closest_t = t_max

    while stack_top > 0 do
        local current = stack[stack_top]
        stack_top = stack_top - 1

        if current.bounds == nil then
            -- skip
        elseif not aabb_intersect(current.bounds, ray.origin, dir_inv, t_min, closest_t) then
            -- skip
        elseif current.object ~= nil then
            -- Leaf node
            local hit = nil
            local obj = current.object
            if obj.type == "sphere" then
                hit = sphere_intersect(obj, ray, t_min, closest_t)
            elseif obj.type == "triangle" then
                hit = triangle_intersect(obj, ray, t_min, closest_t)
            end
            if hit and hit.t < closest_t then
                closest_hit = hit
                closest_t = hit.t
            end
        else
            -- Internal node
            if current.left then
                stack_top = stack_top + 1
                stack[stack_top] = current.left
            end
            if current.right then
                stack_top = stack_top + 1
                stack[stack_top] = current.right
            end
        end
    end

    return closest_hit
end

-- ============================================================================
-- Scene representation
-- ============================================================================

function scene_new()
    return {
        bvh_objects = {},  -- objects that go in BVH (spheres, triangles)
        planes = {},       -- planes (infinite, not in BVH)
        lights = {},       -- point lights
        ambient = color_new(0.05, 0.05, 0.05),
        background = color_new(0.0, 0.0, 0.0),
        bvh = nil
    }
end

function scene_add_object(scene, obj)
    scene.bvh_objects[#scene.bvh_objects + 1] = obj
end

function scene_add_plane(scene, pl)
    scene.planes[#scene.planes + 1] = pl
end

function scene_add_light(scene, position, color_val, intensity)
    scene.lights[#scene.lights + 1] = {
        position = position,
        color = color_val or color_new(1, 1, 1),
        intensity = intensity or 1.0
    }
end

function scene_build_bvh(scene)
    if #scene.bvh_objects > 0 then
        scene.bvh = bvh_build(scene.bvh_objects)
    end
end

-- ============================================================================
-- Scene intersection (BVH + planes)
-- ============================================================================

function scene_intersect(scene, ray, t_min, t_max)
    local closest_hit = nil
    local closest_t = t_max

    -- Check BVH
    if scene.bvh then
        local hit = bvh_intersect(scene.bvh, ray, t_min, closest_t)
        if hit then
            closest_hit = hit
            closest_t = hit.t
        end
    end

    -- Check planes
    for i = 1, #scene.planes do
        local hit = plane_intersect(scene.planes[i], ray, t_min, closest_t)
        if hit then
            closest_hit = hit
            closest_t = hit.t
        end
    end

    return closest_hit
end

-- ============================================================================
-- Shadow testing
-- ============================================================================

function scene_is_shadowed(scene, point, light_pos)
    local to_light = vec3_sub(light_pos, point)
    local dist = vec3_length(to_light)
    local dir = vec3_mul(to_light, 1.0 / dist)
    local shadow_ray = ray_new(point, dir)

    local hit = scene_intersect(scene, shadow_ray, 0.001, dist - 0.001)
    if hit then
        -- If hit object is transparent, partial shadow
        if hit.material and hit.material.transparency > 0.5 then
            return false  -- Let light through transparent objects
        end
        return true
    end
    return false
end

-- ============================================================================
-- Phong shading
-- ============================================================================

function shade_phong(scene, hit, ray, lights)
    local mat = hit.material
    local point = hit.point
    local normal = hit.normal
    local view_dir = vec3_normalize(vec3_negate(ray.direction))

    -- Start with ambient
    local result = color_mul_color(mat.color, scene.ambient)

    for i = 1, #lights do
        local light = lights[i]
        local light_dir = vec3_sub(light.position, point)
        local light_dist = vec3_length(light_dir)
        light_dir = vec3_mul(light_dir, 1.0 / light_dist)

        -- Shadow check
        if not scene_is_shadowed(scene, vec3_add(point, vec3_mul(normal, 0.001)), light.position) then
            -- Diffuse
            local n_dot_l = math_max(0, vec3_dot(normal, light_dir))
            local attenuation = light.intensity / (1.0 + 0.01 * light_dist * light_dist)
            local diffuse = color_mul(color_mul_color(mat.color, light.color), n_dot_l * attenuation)

            -- Specular (Blinn-Phong)
            local half_vec = vec3_normalize(vec3_add(light_dir, view_dir))
            local n_dot_h = math_max(0, vec3_dot(normal, half_vec))
            local spec_strength = mat.specular * (n_dot_h ^ mat.shininess)
            local specular = color_mul(light.color, spec_strength * attenuation)

            result = color_add(result, color_add(diffuse, specular))
        end
    end

    return result
end

-- ============================================================================
-- Fresnel (Schlick's approximation)
-- ============================================================================

function fresnel_schlick(cos_theta, ior)
    local r0 = ((1.0 - ior) / (1.0 + ior))
    r0 = r0 * r0
    return r0 + (1.0 - r0) * ((1.0 - cos_theta) ^ 5)
end

-- ============================================================================
-- Refraction
-- ============================================================================

function refract_ray(incident, normal, ior_ratio)
    local cos_i = -vec3_dot(incident, normal)
    local sin2_t = ior_ratio * ior_ratio * (1.0 - cos_i * cos_i)
    if sin2_t > 1.0 then return nil end -- Total internal reflection
    local cos_t = math_sqrt(1.0 - sin2_t)
    return vec3_add(
        vec3_mul(incident, ior_ratio),
        vec3_mul(normal, ior_ratio * cos_i - cos_t)
    )
end

-- ============================================================================
-- Recursive ray tracing
-- ============================================================================

function trace_ray(scene, ray, depth, max_depth)
    if depth >= max_depth then
        return scene.background
    end

    local hit = scene_intersect(scene, ray, 0.001, math_huge)
    if not hit then
        return scene.background
    end

    local mat = hit.material
    local point = hit.point
    local normal = hit.normal

    -- Base color from Phong shading
    local base_color = shade_phong(scene, hit, ray, scene.lights)

    -- If no reflection or refraction, just return base color
    if mat.reflectivity <= 0.001 and mat.transparency <= 0.001 then
        return base_color
    end

    local result_color = base_color

    -- Reflection
    if mat.reflectivity > 0.001 then
        local reflect_dir = vec3_reflect(ray.direction, normal)
        reflect_dir = vec3_normalize(reflect_dir)
        local reflect_origin = vec3_add(point, vec3_mul(normal, 0.001))
        local reflect_ray = ray_new(reflect_origin, reflect_dir)
        local reflect_color = trace_ray(scene, reflect_ray, depth + 1, max_depth)

        -- Blend reflection with base color
        result_color = color_add(
            color_mul(result_color, 1.0 - mat.reflectivity),
            color_mul(reflect_color, mat.reflectivity)
        )
    end

    -- Refraction (transparency)
    if mat.transparency > 0.001 then
        local cos_i = -vec3_dot(ray.direction, normal)
        local entering = cos_i > 0
        local n = normal
        local ior_ratio

        if entering then
            ior_ratio = 1.0 / mat.ior
        else
            n = vec3_negate(normal)
            cos_i = -cos_i
            ior_ratio = mat.ior
        end

        local refracted = refract_ray(ray.direction, n, ior_ratio)
        if refracted then
            local refract_origin = vec3_sub(point, vec3_mul(n, 0.002))
            refracted = vec3_normalize(refracted)
            local refract_r = ray_new(refract_origin, refracted)
            local refract_color = trace_ray(scene, refract_r, depth + 1, max_depth)

            -- Use Fresnel to blend reflection and refraction
            local fr = fresnel_schlick(math_abs(cos_i), mat.ior)
            result_color = color_add(
                color_mul(result_color, fr + (1.0 - mat.transparency)),
                color_mul(refract_color, mat.transparency * (1.0 - fr))
            )
        end
        -- If total internal reflection, reflection already handled
    end

    return result_color
end

-- ============================================================================
-- Camera
-- ============================================================================

function camera_new(eye, look_at, up, fov, aspect)
    local forward = vec3_normalize(vec3_sub(look_at, eye))
    local right = vec3_normalize(vec3_cross(forward, up))
    local camera_up = vec3_cross(right, forward)

    local half_height = math_tan(fov * math_pi / 360.0)
    local half_width = half_height * aspect

    return {
        eye = eye,
        forward = forward,
        right = right,
        up = camera_up,
        half_width = half_width,
        half_height = half_height
    }
end

function camera_get_ray(cam, u, v)
    -- u, v in [0, 1]
    local x = (2.0 * u - 1.0) * cam.half_width
    local y = (2.0 * v - 1.0) * cam.half_height
    local dir = vec3_normalize({
        x = cam.forward.x + x * cam.right.x + y * cam.up.x,
        y = cam.forward.y + x * cam.right.y + y * cam.up.y,
        z = cam.forward.z + x * cam.right.z + y * cam.up.z
    })
    return ray_new(cam.eye, dir)
end

-- ============================================================================
-- Checkered pattern for planes
-- ============================================================================

function get_checkered_color(point, color1, color2, scale)
    scale = scale or 1.0
    local fx = math_floor(point.x * scale)
    local fz = math_floor(point.z * scale)
    if (fx + fz) % 2 == 0 then
        return color1
    else
        return color2
    end
end

-- ============================================================================
-- Scene: Cornell Box
-- ============================================================================

function create_cornell_box()
    local scene = scene_new()
    scene.background = color_new(0.0, 0.0, 0.0)
    scene.ambient = color_new(0.1, 0.1, 0.1)

    -- Room dimensions: -5 to 5 on x and z, 0 to 10 on y
    local white_mat = material_diffuse(0.73, 0.73, 0.73)
    local red_mat = material_diffuse(0.65, 0.05, 0.05)
    local green_mat = material_diffuse(0.12, 0.45, 0.15)

    -- Floor (y=0)
    local floor_tris = box_new(vec3(-5, -0.1, -5), vec3(5, 0, 5), white_mat)
    for i = 1, #floor_tris do scene_add_object(scene, floor_tris[i]) end

    -- Ceiling (y=10)
    local ceiling_tris = box_new(vec3(-5, 10, -5), vec3(5, 10.1, 5), white_mat)
    for i = 1, #ceiling_tris do scene_add_object(scene, ceiling_tris[i]) end

    -- Back wall (z=-5)
    local back_tris = box_new(vec3(-5, 0, -5.1), vec3(5, 10, -5), white_mat)
    for i = 1, #back_tris do scene_add_object(scene, back_tris[i]) end

    -- Left wall (x=-5) - RED
    local left_tris = box_new(vec3(-5.1, 0, -5), vec3(-5, 10, 5), red_mat)
    for i = 1, #left_tris do scene_add_object(scene, left_tris[i]) end

    -- Right wall (x=5) - GREEN
    local right_tris = box_new(vec3(5, 0, -5), vec3(5.1, 10, 5), green_mat)
    for i = 1, #right_tris do scene_add_object(scene, right_tris[i]) end

    -- Tall box (white)
    local box1_mat = material_diffuse(0.73, 0.73, 0.73)
    local box1_tris = box_new(vec3(-3.5, 0, -3.5), vec3(-1, 6, -1), box1_mat)
    for i = 1, #box1_tris do scene_add_object(scene, box1_tris[i]) end

    -- Short box (white)
    local box2_mat = material_diffuse(0.73, 0.73, 0.73)
    local box2_tris = box_new(vec3(1, 0, -1), vec3(3.5, 3, 2), box2_mat)
    for i = 1, #box2_tris do scene_add_object(scene, box2_tris[i]) end

    -- Ceiling light (area light approximated as point)
    scene_add_light(scene, vec3(0, 9.5, 0), color_new(1, 0.95, 0.8), 80.0)
    -- Slight fill from front
    scene_add_light(scene, vec3(0, 5, 8), color_new(0.5, 0.5, 0.6), 20.0)

    scene_build_bvh(scene)

    local cam = camera_new(
        vec3(0, 5, 14),   -- eye
        vec3(0, 5, 0),    -- look_at
        vec3(0, 1, 0),    -- up
        50,               -- fov
        1.0               -- aspect
    )

    return scene, cam
end

-- ============================================================================
-- Scene: Sphere scene (reflective sphere on checkered plane)
-- ============================================================================

function create_sphere_scene()
    local scene = scene_new()
    scene.background = color_new(0.4, 0.6, 0.9)
    scene.ambient = color_new(0.08, 0.08, 0.1)

    -- Checkered floor plane
    local floor_mat = material_diffuse(0.8, 0.8, 0.8)
    scene_add_plane(scene, plane_new(vec3(0, 0, 0), vec3(0, 1, 0), floor_mat))

    -- Large reflective sphere in center
    local mirror_mat = material_reflective(0.9, 0.9, 0.95, 0.85)
    scene_add_object(scene, sphere_new(vec3(0, 1.5, -2), 1.5, mirror_mat))

    -- Colored spheres around it
    scene_add_object(scene, sphere_new(vec3(-3, 0.8, -1), 0.8, material_diffuse(0.8, 0.2, 0.2)))
    scene_add_object(scene, sphere_new(vec3(3, 0.8, -1), 0.8, material_diffuse(0.2, 0.2, 0.8)))
    scene_add_object(scene, sphere_new(vec3(-1.5, 0.5, 1.5), 0.5, material_diffuse(0.2, 0.8, 0.2)))
    scene_add_object(scene, sphere_new(vec3(1.5, 0.5, 1.5), 0.5, material_diffuse(0.8, 0.8, 0.2)))
    scene_add_object(scene, sphere_new(vec3(0, 0.4, 2.5), 0.4, material_diffuse(0.8, 0.4, 0.8)))

    -- Small reflective spheres
    scene_add_object(scene, sphere_new(vec3(-2, 0.3, 2), 0.3, material_reflective(0.7, 0.7, 0.9, 0.6)))
    scene_add_object(scene, sphere_new(vec3(2, 0.3, 2.5), 0.3, material_reflective(0.9, 0.7, 0.7, 0.6)))

    -- Lights
    scene_add_light(scene, vec3(5, 10, 5), color_new(1, 1, 0.95), 60.0)
    scene_add_light(scene, vec3(-5, 8, 3), color_new(0.6, 0.7, 1.0), 30.0)

    scene_build_bvh(scene)

    local cam = camera_new(
        vec3(0, 4, 8),
        vec3(0, 1, -1),
        vec3(0, 1, 0),
        55,
        1.0
    )

    return scene, cam
end

-- ============================================================================
-- Scene: Glass scene (transparent + mirror spheres)
-- ============================================================================

function create_glass_scene()
    local scene = scene_new()
    scene.background = color_new(0.2, 0.3, 0.5)
    scene.ambient = color_new(0.06, 0.06, 0.08)

    -- Floor
    local floor_mat = material_diffuse(0.6, 0.6, 0.6)
    scene_add_plane(scene, plane_new(vec3(0, 0, 0), vec3(0, 1, 0), floor_mat))

    -- Glass sphere (center)
    local glass_mat = material_glass(0.95, 0.95, 1.0, 1.5)
    scene_add_object(scene, sphere_new(vec3(0, 1.5, -1), 1.5, glass_mat))

    -- Mirror sphere (left)
    local mirror_mat = material_reflective(0.95, 0.95, 0.95, 0.95)
    scene_add_object(scene, sphere_new(vec3(-3.5, 1, -2), 1.0, mirror_mat))

    -- Red sphere (right)
    scene_add_object(scene, sphere_new(vec3(3, 0.8, -0.5), 0.8, material_diffuse(0.85, 0.15, 0.15)))

    -- Small glass sphere
    local glass2 = material_glass(0.9, 1.0, 0.9, 1.3)
    scene_add_object(scene, sphere_new(vec3(1.5, 0.5, 1.5), 0.5, glass2))

    -- Background sphere (big, far away, colored)
    scene_add_object(scene, sphere_new(vec3(0, 3, -12), 4.0, material_diffuse(0.3, 0.5, 0.8)))

    -- Small colored spheres behind glass
    scene_add_object(scene, sphere_new(vec3(-1, 0.4, -3.5), 0.4, material_diffuse(0.9, 0.9, 0.1)))
    scene_add_object(scene, sphere_new(vec3(1, 0.4, -3.5), 0.4, material_diffuse(0.1, 0.9, 0.1)))
    scene_add_object(scene, sphere_new(vec3(0, 0.4, -4.5), 0.4, material_diffuse(0.9, 0.1, 0.9)))

    -- Lights
    scene_add_light(scene, vec3(4, 10, 6), color_new(1, 1, 0.9), 70.0)
    scene_add_light(scene, vec3(-6, 8, 2), color_new(0.7, 0.8, 1.0), 40.0)
    scene_add_light(scene, vec3(0, 12, -4), color_new(1, 1, 1), 30.0)

    scene_build_bvh(scene)

    local cam = camera_new(
        vec3(0, 3.5, 8),
        vec3(0, 1.2, -1),
        vec3(0, 1, 0),
        50,
        1.0
    )

    return scene, cam
end

-- ============================================================================
-- Renderer
-- ============================================================================

function render_scene(scene, cam, width, height, max_depth, use_checker)
    local framebuffer = {}
    local inv_width = 1.0 / width
    local inv_height = 1.0 / height

    for y = 0, height - 1 do
        for x = 0, width - 1 do
            local u = (x + 0.5) * inv_width
            local v = 1.0 - (y + 0.5) * inv_height  -- flip y

            local ray = camera_get_ray(cam, u, v)
            local color = trace_ray(scene, ray, 0, max_depth)

            -- Apply checkered pattern to floor hits if needed
            if use_checker then
                local hit = scene_intersect(scene, ray, 0.001, math_huge)
                if hit and hit.material and hit.normal.y > 0.9 and hit.point.y < 0.01 then
                    local checker = get_checkered_color(hit.point, color_new(0.9, 0.9, 0.9), color_new(0.2, 0.2, 0.2), 1.0)
                    -- Re-shade with checker color
                    local temp_mat = {
                        color = checker,
                        specular = hit.material.specular,
                        reflectivity = hit.material.reflectivity,
                        transparency = hit.material.transparency,
                        ior = hit.material.ior,
                        shininess = hit.material.shininess
                    }
                    local temp_hit = {
                        t = hit.t,
                        point = hit.point,
                        normal = hit.normal,
                        material = temp_mat
                    }
                    color = shade_phong(scene, temp_hit, ray, scene.lights)
                    -- Add reflection for checker floor
                    if temp_mat.reflectivity > 0.001 then
                        local reflect_dir = vec3_reflect(ray.direction, hit.normal)
                        reflect_dir = vec3_normalize(reflect_dir)
                        local reflect_origin = vec3_add(hit.point, vec3_mul(hit.normal, 0.001))
                        local reflect_ray = ray_new(reflect_origin, reflect_dir)
                        local reflect_color = trace_ray(scene, reflect_ray, 1, max_depth)
                        color = color_add(
                            color_mul(color, 1.0 - temp_mat.reflectivity),
                            color_mul(reflect_color, temp_mat.reflectivity)
                        )
                    end
                end
            end

            color = color_clamp(color)
            framebuffer[y * width + x + 1] = color
        end
    end

    return framebuffer
end

-- ============================================================================
-- Checksum computation
-- ============================================================================

function compute_checksum(framebuffer)
    local checksum = 0
    for i = 1, #framebuffer do
        local c = framebuffer[i]
        checksum = checksum + math_floor(c.r * 255) + math_floor(c.g * 255) + math_floor(c.b * 255)
    end
    return checksum
end

-- ============================================================================
-- Additional geometry: Icosphere (more triangles for BVH testing)
-- ============================================================================

function create_icosphere(center, radius, subdivisions, mat)
    -- Start with icosahedron vertices
    local phi = (1.0 + math_sqrt(5.0)) / 2.0

    local raw_verts = {
        vec3(-1, phi, 0), vec3(1, phi, 0), vec3(-1, -phi, 0), vec3(1, -phi, 0),
        vec3(0, -1, phi), vec3(0, 1, phi), vec3(0, -1, -phi), vec3(0, 1, -phi),
        vec3(phi, 0, -1), vec3(phi, 0, 1), vec3(-phi, 0, -1), vec3(-phi, 0, 1)
    }

    -- Normalize vertices to unit sphere
    local verts = {}
    for i = 1, #raw_verts do
        verts[i] = vec3_normalize(raw_verts[i])
    end

    -- Icosahedron faces (1-indexed)
    local faces = {
        {1, 12, 6}, {1, 6, 2}, {1, 2, 8}, {1, 8, 11}, {1, 11, 12},
        {2, 6, 10}, {6, 12, 5}, {12, 11, 3}, {11, 8, 7}, {8, 2, 9},
        {4, 10, 5}, {4, 5, 3}, {4, 3, 7}, {4, 7, 9}, {4, 9, 10},
        {5, 10, 6}, {3, 5, 12}, {7, 3, 11}, {9, 7, 8}, {10, 9, 2}
    }

    -- Subdivide
    for sub = 1, subdivisions do
        local new_faces = {}
        local midpoint_cache = {}

        local function get_midpoint(i1, i2)
            local key
            if i1 < i2 then key = i1 * 10000 + i2
            else key = i2 * 10000 + i1 end

            if midpoint_cache[key] then return midpoint_cache[key] end

            local v1 = verts[i1]
            local v2 = verts[i2]
            local mid = vec3_normalize({
                x = (v1.x + v2.x) * 0.5,
                y = (v1.y + v2.y) * 0.5,
                z = (v1.z + v2.z) * 0.5
            })
            verts[#verts + 1] = mid
            midpoint_cache[key] = #verts
            return #verts
        end

        for i = 1, #faces do
            local f = faces[i]
            local a = get_midpoint(f[1], f[2])
            local b = get_midpoint(f[2], f[3])
            local c = get_midpoint(f[3], f[1])
            new_faces[#new_faces + 1] = {f[1], a, c}
            new_faces[#new_faces + 1] = {f[2], b, a}
            new_faces[#new_faces + 1] = {f[3], c, b}
            new_faces[#new_faces + 1] = {a, b, c}
        end
        faces = new_faces
    end

    -- Generate triangles
    local triangles = {}
    for i = 1, #faces do
        local f = faces[i]
        local v0 = verts[f[1]]
        local v1 = verts[f[2]]
        local v2 = verts[f[3]]
        -- Scale and translate
        local tv0 = vec3_add(center, vec3_mul(v0, radius))
        local tv1 = vec3_add(center, vec3_mul(v1, radius))
        local tv2 = vec3_add(center, vec3_mul(v2, radius))
        triangles[#triangles + 1] = triangle_new(tv0, tv1, tv2, mat)
    end

    return triangles
end

-- ============================================================================
-- Scene: Complex scene with icosphere (more BVH work)
-- ============================================================================

function create_complex_scene()
    local scene = scene_new()
    scene.background = color_new(0.1, 0.1, 0.2)
    scene.ambient = color_new(0.05, 0.05, 0.07)

    -- Floor
    local floor_mat = material_diffuse(0.5, 0.5, 0.5)
    scene_add_plane(scene, plane_new(vec3(0, 0, 0), vec3(0, 1, 0), floor_mat))

    -- Icosphere (many triangles)
    local ico_mat = material_reflective(0.7, 0.3, 0.3, 0.4)
    local ico_tris = create_icosphere(vec3(0, 2, -3), 1.5, 2, ico_mat)
    for i = 1, #ico_tris do scene_add_object(scene, ico_tris[i]) end

    -- Another icosphere (green)
    local ico2_mat = material_diffuse(0.2, 0.7, 0.3)
    local ico2_tris = create_icosphere(vec3(-3, 1.2, -1), 1.0, 2, ico2_mat)
    for i = 1, #ico2_tris do scene_add_object(scene, ico2_tris[i]) end

    -- Glass sphere
    local glass_mat = material_glass(0.95, 0.95, 1.0, 1.5)
    scene_add_object(scene, sphere_new(vec3(3, 1.2, 0), 1.2, glass_mat))

    -- Small spheres scattered
    scene_add_object(scene, sphere_new(vec3(-1.5, 0.4, 1.5), 0.4, material_diffuse(0.9, 0.9, 0.1)))
    scene_add_object(scene, sphere_new(vec3(1, 0.3, 2), 0.3, material_diffuse(0.1, 0.5, 0.9)))
    scene_add_object(scene, sphere_new(vec3(0, 0.5, 3), 0.5, material_reflective(0.8, 0.8, 0.9, 0.7)))

    -- Lights
    scene_add_light(scene, vec3(5, 12, 8), color_new(1, 1, 0.9), 80.0)
    scene_add_light(scene, vec3(-4, 8, 4), color_new(0.6, 0.7, 1.0), 40.0)

    scene_build_bvh(scene)

    local cam = camera_new(
        vec3(0, 4, 9),
        vec3(0, 1.5, -1),
        vec3(0, 1, 0),
        50,
        1.0
    )

    return scene, cam
end

-- ============================================================================
-- Additional helper: Create pyramid from triangles
-- ============================================================================

function create_pyramid(base_center, size, height, mat)
    local half = size * 0.5
    local bx = base_center.x
    local by = base_center.y
    local bz = base_center.z

    local v0 = vec3(bx - half, by, bz - half)
    local v1 = vec3(bx + half, by, bz - half)
    local v2 = vec3(bx + half, by, bz + half)
    local v3 = vec3(bx - half, by, bz + half)
    local apex = vec3(bx, by + height, bz)

    local triangles = {}
    -- Base (2 triangles)
    triangles[#triangles + 1] = triangle_new(v0, v2, v1, mat)
    triangles[#triangles + 1] = triangle_new(v0, v3, v2, mat)
    -- Sides
    triangles[#triangles + 1] = triangle_new(v0, v1, apex, mat)
    triangles[#triangles + 1] = triangle_new(v1, v2, apex, mat)
    triangles[#triangles + 1] = triangle_new(v2, v3, apex, mat)
    triangles[#triangles + 1] = triangle_new(v3, v0, apex, mat)

    return triangles
end

-- ============================================================================
-- Scene: Architectural scene with pyramids and more objects
-- ============================================================================

function create_architectural_scene()
    local scene = scene_new()
    scene.background = color_new(0.5, 0.7, 1.0)
    scene.ambient = color_new(0.1, 0.1, 0.12)

    -- Floor
    local floor_mat = material_diffuse(0.6, 0.55, 0.4)
    scene_add_plane(scene, plane_new(vec3(0, 0, 0), vec3(0, 1, 0), floor_mat))

    -- Pyramids
    local pyramid_mat = material_diffuse(0.8, 0.7, 0.3)
    local pyr1 = create_pyramid(vec3(-3, 0, -4), 3, 3, pyramid_mat)
    for i = 1, #pyr1 do scene_add_object(scene, pyr1[i]) end

    local pyr2_mat = material_diffuse(0.6, 0.6, 0.7)
    local pyr2 = create_pyramid(vec3(3, 0, -5), 2, 4, pyr2_mat)
    for i = 1, #pyr2 do scene_add_object(scene, pyr2[i]) end

    -- Columns (thin tall boxes)
    local col_mat = material_diffuse(0.75, 0.75, 0.7)
    for i = -2, 2 do
        local col = box_new(
            vec3(i * 2.5 - 0.2, 0, 1),
            vec3(i * 2.5 + 0.2, 4, 1.4),
            col_mat
        )
        for j = 1, #col do scene_add_object(scene, col[j]) end
    end

    -- Spheres on top of columns
    for i = -2, 2 do
        local sphere_mat = material_reflective(0.8, 0.6, 0.3, 0.5)
        scene_add_object(scene, sphere_new(vec3(i * 2.5, 4.3, 1.2), 0.3, sphere_mat))
    end

    -- Large reflective sphere
    scene_add_object(scene, sphere_new(vec3(0, 1.5, -1), 1.5, material_reflective(0.85, 0.85, 0.9, 0.8)))

    -- Lights (sun-like)
    scene_add_light(scene, vec3(10, 15, 10), color_new(1, 0.95, 0.8), 120.0)
    scene_add_light(scene, vec3(-5, 8, 8), color_new(0.5, 0.6, 0.8), 40.0)

    scene_build_bvh(scene)

    local cam = camera_new(
        vec3(0, 4, 12),
        vec3(0, 2, -2),
        vec3(0, 1, 0),
        55,
        1.0
    )

    return scene, cam
end

-- ============================================================================
-- Additional geometry helpers for scene variety
-- ============================================================================

function create_disk_triangles(center, radius, normal_dir, segments, mat)
    -- Create a flat disk from triangles
    local n = vec3_normalize(normal_dir)
    -- Find two perpendicular vectors on the disk plane
    local up = vec3(0, 1, 0)
    if math_abs(vec3_dot(n, up)) > 0.99 then
        up = vec3(1, 0, 0)
    end
    local u_axis = vec3_normalize(vec3_cross(n, up))
    local v_axis = vec3_cross(n, u_axis)

    local triangles = {}
    local angle_step = 2.0 * math_pi / segments
    for i = 0, segments - 1 do
        local a1 = i * angle_step
        local a2 = (i + 1) * angle_step
        local p1 = vec3_add(center, vec3_add(vec3_mul(u_axis, radius * math_cos(a1)), vec3_mul(v_axis, radius * math_sin(a1))))
        local p2 = vec3_add(center, vec3_add(vec3_mul(u_axis, radius * math_cos(a2)), vec3_mul(v_axis, radius * math_sin(a2))))
        triangles[#triangles + 1] = triangle_new(center, p1, p2, mat)
    end
    return triangles
end

function create_cylinder_triangles(base_center, radius, height, segments, mat)
    local triangles = {}
    local angle_step = 2.0 * math_pi / segments
    local top_center = vec3_add(base_center, vec3(0, height, 0))

    for i = 0, segments - 1 do
        local a1 = i * angle_step
        local a2 = (i + 1) * angle_step
        local bx1 = base_center.x + radius * math_cos(a1)
        local bz1 = base_center.z + radius * math_sin(a1)
        local bx2 = base_center.x + radius * math_cos(a2)
        local bz2 = base_center.z + radius * math_sin(a2)

        local b1 = vec3(bx1, base_center.y, bz1)
        local b2 = vec3(bx2, base_center.y, bz2)
        local t1 = vec3(bx1, base_center.y + height, bz1)
        local t2 = vec3(bx2, base_center.y + height, bz2)

        -- Side quads (2 triangles each)
        triangles[#triangles + 1] = triangle_new(b1, b2, t2, mat)
        triangles[#triangles + 1] = triangle_new(b1, t2, t1, mat)

        -- Top cap
        triangles[#triangles + 1] = triangle_new(top_center, t1, t2, mat)
        -- Bottom cap
        triangles[#triangles + 1] = triangle_new(base_center, b2, b1, mat)
    end

    return triangles
end

-- ============================================================================
-- Scene: Dense scene with cylinders and more geometry
-- ============================================================================

function create_dense_scene()
    local scene = scene_new()
    scene.background = color_new(0.15, 0.15, 0.25)
    scene.ambient = color_new(0.06, 0.06, 0.08)

    -- Floor
    local floor_mat = material_diffuse(0.4, 0.4, 0.45)
    scene_add_plane(scene, plane_new(vec3(0, 0, 0), vec3(0, 1, 0), floor_mat))

    -- Cylinders in a row
    local cyl_mat1 = material_diffuse(0.7, 0.3, 0.2)
    local cyl_mat2 = material_diffuse(0.2, 0.5, 0.7)
    local cyl_mat3 = material_diffuse(0.5, 0.7, 0.2)

    local cyl1 = create_cylinder_triangles(vec3(-4, 0, -3), 0.5, 3, 8, cyl_mat1)
    for i = 1, #cyl1 do scene_add_object(scene, cyl1[i]) end

    local cyl2 = create_cylinder_triangles(vec3(0, 0, -4), 0.7, 2.5, 8, cyl_mat2)
    for i = 1, #cyl2 do scene_add_object(scene, cyl2[i]) end

    local cyl3 = create_cylinder_triangles(vec3(4, 0, -3), 0.4, 4, 8, cyl_mat3)
    for i = 1, #cyl3 do scene_add_object(scene, cyl3[i]) end

    -- Disks (floating)
    local disk_mat = material_reflective(0.8, 0.6, 0.2, 0.5)
    local disk1 = create_disk_triangles(vec3(-2, 3, -2), 1.0, vec3(0, 1, 0.3), 12, disk_mat)
    for i = 1, #disk1 do scene_add_object(scene, disk1[i]) end

    local disk2 = create_disk_triangles(vec3(2, 2.5, -1), 0.8, vec3(0.2, 1, 0), 12, disk_mat)
    for i = 1, #disk2 do scene_add_object(scene, disk2[i]) end

    -- Glass sphere
    scene_add_object(scene, sphere_new(vec3(0, 1.5, 0), 1.5, material_glass(0.9, 0.95, 1.0, 1.5)))

    -- Mirror sphere
    scene_add_object(scene, sphere_new(vec3(-3, 1, 1), 1.0, material_reflective(0.9, 0.9, 0.95, 0.9)))

    -- Colored spheres
    scene_add_object(scene, sphere_new(vec3(3, 0.6, 1), 0.6, material_diffuse(0.9, 0.2, 0.5)))
    scene_add_object(scene, sphere_new(vec3(1.5, 0.4, 2.5), 0.4, material_diffuse(0.2, 0.9, 0.4)))
    scene_add_object(scene, sphere_new(vec3(-1.5, 0.35, 2.5), 0.35, material_diffuse(0.4, 0.3, 0.9)))

    -- Lights
    scene_add_light(scene, vec3(5, 12, 8), color_new(1, 0.95, 0.85), 90.0)
    scene_add_light(scene, vec3(-6, 9, 5), color_new(0.6, 0.7, 1.0), 50.0)
    scene_add_light(scene, vec3(0, 6, -8), color_new(0.8, 0.8, 0.9), 30.0)

    scene_build_bvh(scene)

    local cam = camera_new(
        vec3(0, 4.5, 9),
        vec3(0, 1.5, -1),
        vec3(0, 1, 0),
        52,
        1.0
    )

    return scene, cam
end

-- ============================================================================
-- Procedural textures and patterns
-- ============================================================================

function pattern_stripe(point, color1, color2, scale)
    scale = scale or 1.0
    local val = math_floor(point.x * scale)
    if val % 2 == 0 then
        return color1
    else
        return color2
    end
end

function pattern_gradient(point, color1, color2, axis, scale)
    scale = scale or 1.0
    local t
    if axis == "x" then t = point.x * scale
    elseif axis == "y" then t = point.y * scale
    else t = point.z * scale end
    t = t - math_floor(t)  -- fract
    return {
        r = color1.r + (color2.r - color1.r) * t,
        g = color1.g + (color2.g - color1.g) * t,
        b = color1.b + (color2.b - color1.b) * t
    }
end

function pattern_ring(point, color1, color2, scale)
    scale = scale or 1.0
    local dist = math_sqrt(point.x * point.x + point.z * point.z) * scale
    local val = math_floor(dist)
    if val % 2 == 0 then
        return color1
    else
        return color2
    end
end

function noise_hash(x, y, z)
    -- Simple integer hash for pseudo-noise
    local n = x * 374761393 + y * 668265263 + z * 1274126177
    n = n % 2147483648
    n = ((n * n) % 2147483648) * 1274126177
    n = n % 2147483648
    return (n % 10000) / 10000.0
end

function noise_smooth(x, y, z)
    local ix = math_floor(x)
    local iy = math_floor(y)
    local iz = math_floor(z)
    local fx = x - ix
    local fy = y - iy
    local fz = z - iz

    -- Smooth interpolation
    fx = fx * fx * (3.0 - 2.0 * fx)
    fy = fy * fy * (3.0 - 2.0 * fy)
    fz = fz * fz * (3.0 - 2.0 * fz)

    local c000 = noise_hash(ix, iy, iz)
    local c100 = noise_hash(ix + 1, iy, iz)
    local c010 = noise_hash(ix, iy + 1, iz)
    local c110 = noise_hash(ix + 1, iy + 1, iz)
    local c001 = noise_hash(ix, iy, iz + 1)
    local c101 = noise_hash(ix + 1, iy, iz + 1)
    local c011 = noise_hash(ix, iy + 1, iz + 1)
    local c111 = noise_hash(ix + 1, iy + 1, iz + 1)

    local c00 = c000 + (c100 - c000) * fx
    local c01 = c001 + (c101 - c001) * fx
    local c10 = c010 + (c110 - c010) * fx
    local c11 = c011 + (c111 - c011) * fx

    local c0 = c00 + (c10 - c00) * fy
    local c1 = c01 + (c11 - c01) * fy

    return c0 + (c1 - c0) * fz
end

function noise_fbm(x, y, z, octaves)
    local value = 0.0
    local amplitude = 1.0
    local frequency = 1.0
    local total_amp = 0.0

    for i = 1, octaves do
        value = value + noise_smooth(x * frequency, y * frequency, z * frequency) * amplitude
        total_amp = total_amp + amplitude
        amplitude = amplitude * 0.5
        frequency = frequency * 2.0
    end

    return value / total_amp
end

function pattern_marble(point, color1, color2, scale)
    scale = scale or 1.0
    local noise_val = noise_fbm(point.x * scale, point.y * scale, point.z * scale, 4)
    local t = (math_sin((point.x + noise_val * 5.0) * scale) + 1.0) * 0.5
    return {
        r = color1.r + (color2.r - color1.r) * t,
        g = color1.g + (color2.g - color1.g) * t,
        b = color1.b + (color2.b - color1.b) * t
    }
end

function pattern_wood(point, color1, color2, scale)
    scale = scale or 1.0
    local dist = math_sqrt(point.x * point.x + point.z * point.z) * scale
    local noise_val = noise_fbm(point.x * 0.5, point.y * 0.5, point.z * 0.5, 3)
    dist = dist + noise_val * 2.0
    local t = (math_sin(dist * math_pi * 2.0) + 1.0) * 0.5
    return {
        r = color1.r + (color2.r - color1.r) * t,
        g = color1.g + (color2.g - color1.g) * t,
        b = color1.b + (color2.b - color1.b) * t
    }
end

-- ============================================================================
-- Tone mapping (Reinhard operator)
-- ============================================================================

function tonemap_reinhard(color)
    return {
        r = color.r / (1.0 + color.r),
        g = color.g / (1.0 + color.g),
        b = color.b / (1.0 + color.b)
    }
end

function tonemap_aces(color)
    -- Approximate ACES filmic curve
    local a = 2.51
    local b = 0.03
    local c = 2.43
    local d = 0.59
    local e = 0.14
    local function aces_channel(x)
        local num = x * (a * x + b)
        local den = x * (c * x + d) + e
        return math_max(0, math_min(1, num / den))
    end
    return {
        r = aces_channel(color.r),
        g = aces_channel(color.g),
        b = aces_channel(color.b)
    }
end

function gamma_correct(color, gamma)
    gamma = gamma or 2.2
    local inv_gamma = 1.0 / gamma
    return {
        r = color.r ^ inv_gamma,
        g = color.g ^ inv_gamma,
        b = color.b ^ inv_gamma
    }
end

-- ============================================================================
-- Post-processing: apply tone mapping and gamma to framebuffer
-- ============================================================================

function post_process_framebuffer(framebuffer, use_aces)
    local result = {}
    for i = 1, #framebuffer do
        local c = framebuffer[i]
        if use_aces then
            c = tonemap_aces(c)
        else
            c = tonemap_reinhard(c)
        end
        c = gamma_correct(c, 2.2)
        c = color_clamp(c)
        result[i] = c
    end
    return result
end

-- ============================================================================
-- Scene: Textured scene (uses procedural patterns)
-- ============================================================================

function create_textured_scene()
    local scene = scene_new()
    scene.background = color_new(0.3, 0.4, 0.6)
    scene.ambient = color_new(0.08, 0.08, 0.1)

    -- Floor with marble-like material
    local floor_mat = material_diffuse(0.7, 0.7, 0.65)
    scene_add_plane(scene, plane_new(vec3(0, 0, 0), vec3(0, 1, 0), floor_mat))

    -- Large sphere with wood-like coloring (computed at shade time via diffuse approx)
    local wood_sphere_mat = material_diffuse(0.6, 0.4, 0.2)
    scene_add_object(scene, sphere_new(vec3(-2, 1.5, -2), 1.5, wood_sphere_mat))

    -- Marble-colored sphere
    local marble_mat = material_new(color_new(0.85, 0.85, 0.8), 0.5, 0.2, 0.0, 1.5, 48)
    scene_add_object(scene, sphere_new(vec3(2, 1.2, -1), 1.2, marble_mat))

    -- Striped sphere (approximate by material)
    local stripe_mat = material_diffuse(0.3, 0.5, 0.8)
    scene_add_object(scene, sphere_new(vec3(0, 0.8, 1.5), 0.8, stripe_mat))

    -- Ring-patterned sphere
    local ring_mat = material_new(color_new(0.7, 0.5, 0.3), 0.4, 0.1, 0.0, 1.5, 32)
    scene_add_object(scene, sphere_new(vec3(-3.5, 0.7, 1), 0.7, ring_mat))

    -- Metallic sphere
    local metallic = material_reflective(0.85, 0.75, 0.5, 0.7)
    scene_add_object(scene, sphere_new(vec3(3.5, 0.9, 0.5), 0.9, metallic))

    -- Small bright spheres
    scene_add_object(scene, sphere_new(vec3(-1, 0.3, 3), 0.3, material_diffuse(0.95, 0.1, 0.1)))
    scene_add_object(scene, sphere_new(vec3(0.5, 0.3, 3.5), 0.3, material_diffuse(0.1, 0.95, 0.1)))
    scene_add_object(scene, sphere_new(vec3(2, 0.3, 3), 0.3, material_diffuse(0.1, 0.1, 0.95)))

    -- Icosphere in background
    local ico_mat = material_diffuse(0.6, 0.6, 0.7)
    local ico_tris = create_icosphere(vec3(0, 3, -6), 2.0, 2, ico_mat)
    for i = 1, #ico_tris do scene_add_object(scene, ico_tris[i]) end

    -- Lights
    scene_add_light(scene, vec3(6, 10, 6), color_new(1, 0.95, 0.85), 70.0)
    scene_add_light(scene, vec3(-4, 8, 4), color_new(0.6, 0.7, 1.0), 35.0)
    scene_add_light(scene, vec3(0, 12, -2), color_new(0.9, 0.9, 1.0), 25.0)

    scene_build_bvh(scene)

    local cam = camera_new(
        vec3(0, 3.5, 8),
        vec3(0, 1.2, -1),
        vec3(0, 1, 0),
        55,
        1.0
    )

    return scene, cam
end

-- ============================================================================
-- Scene: Multi-light scene (stress test shadows)
-- ============================================================================

function create_multilight_scene()
    local scene = scene_new()
    scene.background = color_new(0.02, 0.02, 0.05)
    scene.ambient = color_new(0.02, 0.02, 0.03)

    -- Floor
    local floor_mat = material_diffuse(0.5, 0.5, 0.5)
    scene_add_plane(scene, plane_new(vec3(0, 0, 0), vec3(0, 1, 0), floor_mat))

    -- Central reflective sphere
    local center_mat = material_reflective(0.9, 0.9, 0.95, 0.8)
    scene_add_object(scene, sphere_new(vec3(0, 2, 0), 2.0, center_mat))

    -- Surrounding smaller spheres
    local num_ring = 8
    for i = 0, num_ring - 1 do
        local angle = (i / num_ring) * 2.0 * math_pi
        local x = 4.0 * math_cos(angle)
        local z = 4.0 * math_sin(angle)
        local r = (i % 3 == 0) and 0.6 or 0.4
        local mat
        if i % 3 == 0 then
            mat = material_diffuse(0.8, 0.2, 0.2)
        elseif i % 3 == 1 then
            mat = material_diffuse(0.2, 0.8, 0.2)
        else
            mat = material_diffuse(0.2, 0.2, 0.8)
        end
        scene_add_object(scene, sphere_new(vec3(x, r, z), r, mat))
    end

    -- Many colored lights
    scene_add_light(scene, vec3(5, 8, 5), color_new(1, 0.3, 0.3), 40.0)
    scene_add_light(scene, vec3(-5, 8, 5), color_new(0.3, 1, 0.3), 40.0)
    scene_add_light(scene, vec3(5, 8, -5), color_new(0.3, 0.3, 1), 40.0)
    scene_add_light(scene, vec3(-5, 8, -5), color_new(1, 1, 0.3), 40.0)
    scene_add_light(scene, vec3(0, 12, 0), color_new(1, 1, 1), 50.0)
    scene_add_light(scene, vec3(0, 3, 8), color_new(0.5, 0.5, 0.8), 20.0)

    scene_build_bvh(scene)

    local cam = camera_new(
        vec3(0, 6, 10),
        vec3(0, 1.5, 0),
        vec3(0, 1, 0),
        50,
        1.0
    )

    return scene, cam
end

-- ============================================================================
-- Scene: Depth-of-field approximation (multiple jittered rays per pixel)
-- ============================================================================

function render_scene_dof(scene, cam, width, height, max_depth, aperture, focus_dist)
    local framebuffer = {}
    local inv_width = 1.0 / width
    local inv_height = 1.0 / height
    local samples = 4  -- 4 samples per pixel for DOF

    -- Simple deterministic jitter
    local offsets = {
        {dx = -0.25, dy = -0.25},
        {dx = 0.25, dy = -0.25},
        {dx = -0.25, dy = 0.25},
        {dx = 0.25, dy = 0.25}
    }

    for y = 0, height - 1 do
        for x = 0, width - 1 do
            local total_r = 0
            local total_g = 0
            local total_b = 0

            for s = 1, samples do
                local u = (x + 0.5 + offsets[s].dx * 0.5) * inv_width
                local v = 1.0 - (y + 0.5 + offsets[s].dy * 0.5) * inv_height

                -- Generate ray with DOF offset
                local base_ray = camera_get_ray(cam, u, v)
                local focus_point = ray_point_at(base_ray, focus_dist)

                -- Offset origin on lens
                local lens_u = offsets[s].dx * aperture
                local lens_v = offsets[s].dy * aperture
                local offset_origin = vec3_add(base_ray.origin,
                    vec3_add(vec3_mul(cam.right, lens_u), vec3_mul(cam.up, lens_v)))
                local new_dir = vec3_normalize(vec3_sub(focus_point, offset_origin))
                local dof_ray = ray_new(offset_origin, new_dir)

                local color = trace_ray(scene, dof_ray, 0, max_depth)
                total_r = total_r + color.r
                total_g = total_g + color.g
                total_b = total_b + color.b
            end

            local inv_samples = 1.0 / samples
            local final_color = color_clamp({
                r = total_r * inv_samples,
                g = total_g * inv_samples,
                b = total_b * inv_samples
            })
            framebuffer[y * width + x + 1] = final_color
        end
    end

    return framebuffer
end

-- ============================================================================
-- Scene: DOF scene (for depth-of-field rendering)
-- ============================================================================

function create_dof_scene()
    local scene = scene_new()
    scene.background = color_new(0.3, 0.4, 0.7)
    scene.ambient = color_new(0.08, 0.08, 0.1)

    -- Floor
    local floor_mat = material_diffuse(0.5, 0.5, 0.45)
    scene_add_plane(scene, plane_new(vec3(0, 0, 0), vec3(0, 1, 0), floor_mat))

    -- Row of spheres at different depths
    local depths = {-8, -5, -2, 1, 4}
    local colors_list = {
        {0.9, 0.2, 0.2},
        {0.2, 0.9, 0.2},
        {0.2, 0.2, 0.9},
        {0.9, 0.9, 0.2},
        {0.9, 0.2, 0.9}
    }
    for i = 1, #depths do
        local c = colors_list[i]
        local mat = material_diffuse(c[1], c[2], c[3])
        scene_add_object(scene, sphere_new(vec3((i - 3) * 2.5, 1, depths[i]), 1.0, mat))
    end

    -- Reflective sphere in focus
    scene_add_object(scene, sphere_new(vec3(0, 1.5, -2), 1.5, material_reflective(0.8, 0.8, 0.9, 0.6)))

    -- Lights
    scene_add_light(scene, vec3(5, 10, 5), color_new(1, 1, 0.9), 60.0)
    scene_add_light(scene, vec3(-3, 8, -3), color_new(0.7, 0.7, 1.0), 30.0)

    scene_build_bvh(scene)

    local cam = camera_new(
        vec3(0, 4, 8),
        vec3(0, 1, -2),
        vec3(0, 1, 0),
        50,
        1.0
    )

    return scene, cam
end

-- ============================================================================
-- Supersampling anti-aliasing renderer (2x2)
-- ============================================================================

function render_scene_aa(scene, cam, width, height, max_depth, use_checker)
    local framebuffer = {}
    local inv_width = 1.0 / width
    local inv_height = 1.0 / height

    local sub_offsets = {
        {0.25, 0.25},
        {0.75, 0.25},
        {0.25, 0.75},
        {0.75, 0.75}
    }

    for y = 0, height - 1 do
        for x = 0, width - 1 do
            local total_r = 0
            local total_g = 0
            local total_b = 0

            for s = 1, 4 do
                local u = (x + sub_offsets[s][1]) * inv_width
                local v = 1.0 - (y + sub_offsets[s][2]) * inv_height
                local ray = camera_get_ray(cam, u, v)
                local color = trace_ray(scene, ray, 0, max_depth)

                -- Apply checkered pattern
                if use_checker then
                    local hit = scene_intersect(scene, ray, 0.001, math_huge)
                    if hit and hit.material and hit.normal.y > 0.9 and hit.point.y < 0.01 then
                        local checker = get_checkered_color(hit.point, color_new(0.9, 0.9, 0.9), color_new(0.2, 0.2, 0.2), 1.0)
                        local temp_mat = {
                            color = checker,
                            specular = hit.material.specular,
                            reflectivity = hit.material.reflectivity,
                            transparency = hit.material.transparency,
                            ior = hit.material.ior,
                            shininess = hit.material.shininess
                        }
                        local temp_hit = {
                            t = hit.t,
                            point = hit.point,
                            normal = hit.normal,
                            material = temp_mat
                        }
                        color = shade_phong(scene, temp_hit, ray, scene.lights)
                    end
                end

                total_r = total_r + color.r
                total_g = total_g + color.g
                total_b = total_b + color.b
            end

            local final_color = color_clamp({
                r = total_r * 0.25,
                g = total_g * 0.25,
                b = total_b * 0.25
            })
            framebuffer[y * width + x + 1] = final_color
        end
    end

    return framebuffer
end

-- ============================================================================
-- Matrix operations for object transforms
-- ============================================================================

function mat4_identity()
    return {
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1
    }
end

function mat4_translate(tx, ty, tz)
    return {
        1, 0, 0, tx,
        0, 1, 0, ty,
        0, 0, 1, tz,
        0, 0, 0, 1
    }
end

function mat4_scale(sx, sy, sz)
    return {
        sx, 0, 0, 0,
        0, sy, 0, 0,
        0, 0, sz, 0,
        0, 0, 0, 1
    }
end

function mat4_rotate_y(angle)
    local c = math_cos(angle)
    local s = math_sin(angle)
    return {
        c, 0, s, 0,
        0, 1, 0, 0,
        -s, 0, c, 0,
        0, 0, 0, 1
    }
end

function mat4_rotate_x(angle)
    local c = math_cos(angle)
    local s = math_sin(angle)
    return {
        1, 0, 0, 0,
        0, c, -s, 0,
        0, s, c, 0,
        0, 0, 0, 1
    }
end

function mat4_rotate_z(angle)
    local c = math_cos(angle)
    local s = math_sin(angle)
    return {
        c, -s, 0, 0,
        s, c, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1
    }
end

function mat4_mul(a, b)
    local result = {}
    for row = 0, 3 do
        for col = 0, 3 do
            local sum = 0
            for k = 0, 3 do
                sum = sum + a[row * 4 + k + 1] * b[k * 4 + col + 1]
            end
            result[row * 4 + col + 1] = sum
        end
    end
    return result
end

function mat4_transform_point(m, p)
    return {
        x = m[1] * p.x + m[2] * p.y + m[3] * p.z + m[4],
        y = m[5] * p.x + m[6] * p.y + m[7] * p.z + m[8],
        z = m[9] * p.x + m[10] * p.y + m[11] * p.z + m[12]
    }
end

function mat4_transform_direction(m, d)
    return vec3_normalize({
        x = m[1] * d.x + m[2] * d.y + m[3] * d.z,
        y = m[5] * d.x + m[6] * d.y + m[7] * d.z,
        z = m[9] * d.x + m[10] * d.y + m[11] * d.z
    })
end

-- ============================================================================
-- Create rotated box using matrix transform
-- ============================================================================

function create_rotated_box(center, half_extents, rotation_y, mat)
    local hx = half_extents.x
    local hy = half_extents.y
    local hz = half_extents.z

    -- 8 corners of the box before rotation
    local corners = {
        vec3(-hx, -hy, -hz), vec3(hx, -hy, -hz),
        vec3(hx, hy, -hz), vec3(-hx, hy, -hz),
        vec3(-hx, -hy, hz), vec3(hx, -hy, hz),
        vec3(hx, hy, hz), vec3(-hx, hy, hz)
    }

    -- Apply rotation and translation
    local rot = mat4_rotate_y(rotation_y)
    local trans = mat4_translate(center.x, center.y, center.z)
    local xform = mat4_mul(trans, rot)

    local transformed = {}
    for i = 1, 8 do
        transformed[i] = mat4_transform_point(xform, corners[i])
    end

    -- Create 12 triangles (6 faces, 2 tris each)
    local triangles = {}
    -- Face indices (1-indexed)
    local face_indices = {
        {1, 2, 3, 4}, -- front (was -z, now depends on rotation)
        {5, 8, 7, 6}, -- back
        {4, 3, 7, 8}, -- top
        {1, 5, 6, 2}, -- bottom
        {2, 6, 7, 3}, -- right
        {1, 4, 8, 5}  -- left
    }

    for i = 1, #face_indices do
        local f = face_indices[i]
        triangles[#triangles + 1] = triangle_new(transformed[f[1]], transformed[f[2]], transformed[f[3]], mat)
        triangles[#triangles + 1] = triangle_new(transformed[f[1]], transformed[f[3]], transformed[f[4]], mat)
    end

    return triangles
end

-- ============================================================================
-- Scene: Rotated boxes (Cornell box variant with rotated inner boxes)
-- ============================================================================

function create_rotated_box_scene()
    local scene = scene_new()
    scene.background = color_new(0.0, 0.0, 0.0)
    scene.ambient = color_new(0.08, 0.08, 0.08)

    -- Room walls
    local white_mat = material_diffuse(0.73, 0.73, 0.73)
    local red_mat = material_diffuse(0.65, 0.05, 0.05)
    local green_mat = material_diffuse(0.12, 0.45, 0.15)
    local blue_mat = material_diffuse(0.1, 0.1, 0.6)

    -- Floor
    local floor_tris = box_new(vec3(-5, -0.1, -5), vec3(5, 0, 5), white_mat)
    for i = 1, #floor_tris do scene_add_object(scene, floor_tris[i]) end

    -- Ceiling
    local ceil_tris = box_new(vec3(-5, 10, -5), vec3(5, 10.1, 5), white_mat)
    for i = 1, #ceil_tris do scene_add_object(scene, ceil_tris[i]) end

    -- Back wall
    local back_tris = box_new(vec3(-5, 0, -5.1), vec3(5, 10, -5), blue_mat)
    for i = 1, #back_tris do scene_add_object(scene, back_tris[i]) end

    -- Left wall (red)
    local left_tris = box_new(vec3(-5.1, 0, -5), vec3(-5, 10, 5), red_mat)
    for i = 1, #left_tris do scene_add_object(scene, left_tris[i]) end

    -- Right wall (green)
    local right_tris = box_new(vec3(5, 0, -5), vec3(5.1, 10, 5), green_mat)
    for i = 1, #right_tris do scene_add_object(scene, right_tris[i]) end

    -- Rotated tall box
    local box1_tris = create_rotated_box(
        vec3(-2, 3, -2),
        vec3(1.2, 3, 1.2),
        0.3,  -- ~17 degrees rotation
        white_mat
    )
    for i = 1, #box1_tris do scene_add_object(scene, box1_tris[i]) end

    -- Rotated short box
    local box2_tris = create_rotated_box(
        vec3(2, 1.5, 1),
        vec3(1.2, 1.5, 1.2),
        -0.25,  -- ~-14 degrees rotation
        white_mat
    )
    for i = 1, #box2_tris do scene_add_object(scene, box2_tris[i]) end

    -- Reflective sphere on short box
    scene_add_object(scene, sphere_new(vec3(2, 3.5, 1), 0.7, material_reflective(0.9, 0.9, 0.95, 0.85)))

    -- Light
    scene_add_light(scene, vec3(0, 9.5, 0), color_new(1, 0.95, 0.8), 90.0)

    scene_build_bvh(scene)

    local cam = camera_new(
        vec3(0, 5, 14),
        vec3(0, 5, 0),
        vec3(0, 1, 0),
        50,
        1.0
    )

    return scene, cam
end

-- ============================================================================
-- Statistics: compute image statistics for validation
-- ============================================================================

function compute_image_stats(framebuffer)
    local min_r, min_g, min_b = 1, 1, 1
    local max_r, max_g, max_b = 0, 0, 0
    local sum_r, sum_g, sum_b = 0, 0, 0
    local count = #framebuffer

    for i = 1, count do
        local c = framebuffer[i]
        if c.r < min_r then min_r = c.r end
        if c.g < min_g then min_g = c.g end
        if c.b < min_b then min_b = c.b end
        if c.r > max_r then max_r = c.r end
        if c.g > max_g then max_g = c.g end
        if c.b > max_b then max_b = c.b end
        sum_r = sum_r + c.r
        sum_g = sum_g + c.g
        sum_b = sum_b + c.b
    end

    return {
        min = {r = min_r, g = min_g, b = min_b},
        max = {r = max_r, g = max_g, b = max_b},
        avg = {r = sum_r / count, g = sum_g / count, b = sum_b / count}
    }
end

-- ============================================================================
-- Variance computation for adaptive sampling hints
-- ============================================================================

function compute_variance(framebuffer, width, height)
    local total_variance = 0
    local count = 0

    for y = 1, height - 2 do
        for x = 1, width - 2 do
            local idx = y * width + x + 1
            local c = framebuffer[idx]
            local lum = 0.2126 * c.r + 0.7152 * c.g + 0.0722 * c.b

            -- Compare with neighbors
            local left = framebuffer[y * width + (x - 1) + 1]
            local right = framebuffer[y * width + (x + 1) + 1]
            local up_pixel = framebuffer[(y - 1) * width + x + 1]
            local down = framebuffer[(y + 1) * width + x + 1]

            local lum_l = 0.2126 * left.r + 0.7152 * left.g + 0.0722 * left.b
            local lum_r = 0.2126 * right.r + 0.7152 * right.g + 0.0722 * right.b
            local lum_u = 0.2126 * up_pixel.r + 0.7152 * up_pixel.g + 0.0722 * up_pixel.b
            local lum_d = 0.2126 * down.r + 0.7152 * down.g + 0.0722 * down.b

            local diff = math_abs(lum - lum_l) + math_abs(lum - lum_r) + math_abs(lum - lum_u) + math_abs(lum - lum_d)
            total_variance = total_variance + diff
            count = count + 1
        end
    end

    return total_variance / count
end

-- ============================================================================
-- Rendering configuration
-- ============================================================================

local RENDER_WIDTH = 48
local RENDER_HEIGHT = 48
local MAX_DEPTH = 3
local USE_CHECKER_FLOOR = true

-- ============================================================================
-- Main benchmark
-- ============================================================================

function run_benchmark()
    local total_checksum = 0
    local iteration_count = 0

    -- Scene 1: Cornell Box
    local scene1, cam1 = create_cornell_box()
    local fb1 = render_scene(scene1, cam1, RENDER_WIDTH, RENDER_HEIGHT, MAX_DEPTH, false)
    local cs1 = compute_checksum(fb1)
    total_checksum = total_checksum + cs1
    iteration_count = iteration_count + 1

    -- Scene 2: Sphere scene with checkered floor
    local scene2, cam2 = create_sphere_scene()
    local fb2 = render_scene(scene2, cam2, RENDER_WIDTH, RENDER_HEIGHT, MAX_DEPTH, USE_CHECKER_FLOOR)
    local cs2 = compute_checksum(fb2)
    total_checksum = total_checksum + cs2
    iteration_count = iteration_count + 1

    -- Scene 3: Glass scene
    local scene3, cam3 = create_glass_scene()
    local fb3 = render_scene(scene3, cam3, RENDER_WIDTH, RENDER_HEIGHT, MAX_DEPTH, false)
    local cs3 = compute_checksum(fb3)
    total_checksum = total_checksum + cs3
    iteration_count = iteration_count + 1

    -- Scene 4: Complex scene with icospheres
    local scene4, cam4 = create_complex_scene()
    local fb4 = render_scene(scene4, cam4, RENDER_WIDTH, RENDER_HEIGHT, MAX_DEPTH, USE_CHECKER_FLOOR)
    local cs4 = compute_checksum(fb4)
    total_checksum = total_checksum + cs4
    iteration_count = iteration_count + 1

    -- Scene 5: Architectural scene
    local scene5, cam5 = create_architectural_scene()
    local fb5 = render_scene(scene5, cam5, RENDER_WIDTH, RENDER_HEIGHT, MAX_DEPTH, USE_CHECKER_FLOOR)
    local cs5 = compute_checksum(fb5)
    total_checksum = total_checksum + cs5
    iteration_count = iteration_count + 1

    -- Scene 6: Dense scene
    local scene6, cam6 = create_dense_scene()
    local fb6 = render_scene(scene6, cam6, RENDER_WIDTH, RENDER_HEIGHT, MAX_DEPTH, false)
    local cs6 = compute_checksum(fb6)
    total_checksum = total_checksum + cs6
    iteration_count = iteration_count + 1

    -- Scene 7: Textured scene
    local scene7, cam7 = create_textured_scene()
    local fb7 = render_scene(scene7, cam7, RENDER_WIDTH, RENDER_HEIGHT, MAX_DEPTH, USE_CHECKER_FLOOR)
    local cs7 = compute_checksum(fb7)
    total_checksum = total_checksum + cs7
    iteration_count = iteration_count + 1

    -- Scene 8: Multi-light scene
    local scene8, cam8 = create_multilight_scene()
    local fb8 = render_scene(scene8, cam8, RENDER_WIDTH, RENDER_HEIGHT, MAX_DEPTH, false)
    local cs8 = compute_checksum(fb8)
    total_checksum = total_checksum + cs8
    iteration_count = iteration_count + 1

    -- Scene 9: DOF scene (with depth of field rendering)
    local scene9, cam9 = create_dof_scene()
    local fb9 = render_scene_dof(scene9, cam9, RENDER_WIDTH, RENDER_HEIGHT, MAX_DEPTH, 0.05, 10.0)
    local cs9 = compute_checksum(fb9)
    total_checksum = total_checksum + cs9
    iteration_count = iteration_count + 1

    -- Scene 10: Rotated box scene
    local scene10, cam10 = create_rotated_box_scene()
    local fb10 = render_scene(scene10, cam10, RENDER_WIDTH, RENDER_HEIGHT, MAX_DEPTH, false)
    local cs10 = compute_checksum(fb10)
    total_checksum = total_checksum + cs10
    iteration_count = iteration_count + 1

    -- Post-process scene 3 with tone mapping for extra work
    local fb3_tonemapped = post_process_framebuffer(fb3, true)
    local cs3t = compute_checksum(fb3_tonemapped)
    total_checksum = total_checksum + cs3t
    iteration_count = iteration_count + 1

    -- Compute variance stats on scene 2 for extra computation
    local var2 = compute_variance(fb2, RENDER_WIDTH, RENDER_HEIGHT)
    total_checksum = total_checksum + math_floor(var2 * 1000)
    iteration_count = iteration_count + 1

    return total_checksum, iteration_count
end

-- ============================================================================
-- Timing loop
-- ============================================================================

-- First, do a calibration run to get the reference checksum
local checksum, iterations = run_benchmark()
if checksum ~= 16019469 then
    error("Bad checksum " .. checksum)
end
if iterations ~= 12 then
    error("Wrong number of iterations " .. iterations)
end

end

bench.runCode(test, "raytrace")
