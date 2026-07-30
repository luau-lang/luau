local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()


-- 2D Physics Engine Benchmark
-- A rigid body dynamics simulation with broad-phase (spatial hash) and narrow-phase
-- (SAT) collision detection, sequential impulse constraint solver, joints, and friction.
-- Style: vectors as plain tables, mix of local functions and upvalues, math-heavy.

local math_sqrt = math.sqrt
local math_abs = math.abs
local math_min = math.min
local math_max = math.max
local math_cos = math.cos
local math_sin = math.sin
local math_atan2 = math.atan2 or math.atan
local math_pi = math.pi
local math_huge = math.huge
local math_floor = math.floor

-- Deterministic PRNG
local prng_state = 12345
local function random()
    prng_state = (prng_state * 1103515245 + 12345) % 2147483648
    return prng_state / 2147483648
end

local function randomRange(lo, hi)
    return lo + random() * (hi - lo)
end

local function resetRandom()
    prng_state = 12345
end

-- ============================================================================
-- Vector operations (no metatables - just functions on {x, y} tables)
-- ============================================================================

local function vec(x, y)
    return {x = x, y = y}
end

local function vecAdd(a, b)
    return {x = a.x + b.x, y = a.y + b.y}
end

local function vecSub(a, b)
    return {x = a.x - b.x, y = a.y - b.y}
end

local function vecMul(v, s)
    return {x = v.x * s, y = v.y * s}
end

local function vecDiv(v, s)
    return {x = v.x / s, y = v.y / s}
end

local function vecDot(a, b)
    return a.x * b.x + a.y * b.y
end

local function vecCross(a, b)
    return a.x * b.y - a.y * b.x
end

local function vecCrossScalar(v, s)
    return {x = -s * v.y, y = s * v.x}
end

local function scalarCrossVec(s, v)
    return {x = -s * v.y, y = s * v.x}
end

local function vecLen(v)
    return math_sqrt(v.x * v.x + v.y * v.y)
end

local function vecLenSq(v)
    return v.x * v.x + v.y * v.y
end

local function vecNormalize(v)
    local len = math_sqrt(v.x * v.x + v.y * v.y)
    if len < 1e-10 then return {x = 0, y = 0} end
    return {x = v.x / len, y = v.y / len}
end

local function vecNeg(v)
    return {x = -v.x, y = -v.y}
end

local function vecPerp(v)
    return {x = -v.y, y = v.x}
end

local function vecRotate(v, angle)
    local c = math_cos(angle)
    local s = math_sin(angle)
    return {x = v.x * c - v.y * s, y = v.x * s + v.y * c}
end

local function vecLerp(a, b, t)
    return {x = a.x + (b.x - a.x) * t, y = a.y + (b.y - a.y) * t}
end

local function vecDist(a, b)
    local dx = b.x - a.x
    local dy = b.y - a.y
    return math_sqrt(dx * dx + dy * dy)
end

local function vecDistSq(a, b)
    local dx = b.x - a.x
    local dy = b.y - a.y
    return dx * dx + dy * dy
end

local function vecClamp(v, maxLen)
    local lenSq = v.x * v.x + v.y * v.y
    if lenSq > maxLen * maxLen then
        local len = math_sqrt(lenSq)
        return {x = v.x * maxLen / len, y = v.y * maxLen / len}
    end
    return v
end

local function vecEqual(a, b, eps)
    eps = eps or 1e-6
    return math_abs(a.x - b.x) < eps and math_abs(a.y - b.y) < eps
end

-- ============================================================================
-- Matrix 2x2 operations (for rotations)
-- ============================================================================

local function mat2(angle)
    local c = math_cos(angle)
    local s = math_sin(angle)
    return {m00 = c, m01 = -s, m10 = s, m11 = c}
end

local function mat2MulVec(m, v)
    return {x = m.m00 * v.x + m.m01 * v.y, y = m.m10 * v.x + m.m11 * v.y}
end

local function mat2Transpose(m)
    return {m00 = m.m00, m01 = m.m10, m10 = m.m01, m11 = m.m11}
end

-- ============================================================================
-- Shape definitions
-- ============================================================================

local SHAPE_CIRCLE = 1
local SHAPE_POLYGON = 2

local function createCircle(radius)
    return {
        type = SHAPE_CIRCLE,
        radius = radius,
        area = math_pi * radius * radius
    }
end

local function computePolygonArea(vertices)
    local area = 0
    local n = #vertices
    for i = 1, n do
        local j = (i % n) + 1
        area = area + vertices[i].x * vertices[j].y
        area = area - vertices[j].x * vertices[i].y
    end
    return math_abs(area) / 2
end

local function computePolygonCentroid(vertices)
    local cx, cy = 0, 0
    local n = #vertices
    local area = 0
    for i = 1, n do
        local j = (i % n) + 1
        local cross = vertices[i].x * vertices[j].y - vertices[j].x * vertices[i].y
        area = area + cross
        cx = cx + (vertices[i].x + vertices[j].x) * cross
        cy = cy + (vertices[i].y + vertices[j].y) * cross
    end
    area = area / 2
    if math_abs(area) < 1e-10 then return vec(0, 0) end
    cx = cx / (6 * area)
    cy = cy / (6 * area)
    return vec(cx, cy)
end

local function computePolygonMOI(vertices, mass)
    local n = #vertices
    local numerator = 0
    local denominator = 0
    for i = 1, n do
        local j = (i % n) + 1
        local vi = vertices[i]
        local vj = vertices[j]
        local cross = math_abs(vecCross(vi, vj))
        numerator = numerator + cross * (vecDot(vi, vi) + vecDot(vi, vj) + vecDot(vj, vj))
        denominator = denominator + cross
    end
    if denominator < 1e-10 then return mass end
    return mass * numerator / (6 * denominator)
end

local function computePolygonNormals(vertices)
    local normals = {}
    local n = #vertices
    for i = 1, n do
        local j = (i % n) + 1
        local edge = vecSub(vertices[j], vertices[i])
        local normal = vecNormalize(vecPerp(edge))
        normals[i] = normal
    end
    return normals
end

local function createPolygon(vertices)
    local centroid = computePolygonCentroid(vertices)
    local centered = {}
    for i = 1, #vertices do
        centered[i] = vecSub(vertices[i], centroid)
    end
    local normals = computePolygonNormals(centered)
    local area = computePolygonArea(centered)
    return {
        type = SHAPE_POLYGON,
        vertices = centered,
        normals = normals,
        vertexCount = #centered,
        area = area,
        centroidOffset = centroid
    }
end

local function createBox(halfWidth, halfHeight)
    local vertices = {
        vec(-halfWidth, -halfHeight),
        vec(halfWidth, -halfHeight),
        vec(halfWidth, halfHeight),
        vec(-halfWidth, halfHeight)
    }
    return createPolygon(vertices)
end

local function createRegularPolygon(radius, sides)
    local vertices = {}
    for i = 1, sides do
        local angle = (i - 1) * 2 * math_pi / sides - math_pi / 2
        vertices[i] = vec(radius * math_cos(angle), radius * math_sin(angle))
    end
    return createPolygon(vertices)
end

-- ============================================================================
-- Rigid Body
-- ============================================================================

local bodyIdCounter = 0

local function createBody(shape, x, y, density, isStatic)
    bodyIdCounter = bodyIdCounter + 1
    local mass, invMass, inertia, invInertia
    if isStatic then
        mass = 0
        invMass = 0
        inertia = 0
        invInertia = 0
    else
        mass = shape.area * density
        invMass = 1 / mass
        if shape.type == SHAPE_CIRCLE then
            inertia = 0.5 * mass * shape.radius * shape.radius
        else
            inertia = computePolygonMOI(shape.vertices, mass)
        end
        invInertia = 1 / inertia
    end

    return {
        id = bodyIdCounter,
        shape = shape,
        position = vec(x, y),
        velocity = vec(0, 0),
        angle = 0,
        angularVelocity = 0,
        force = vec(0, 0),
        torque = 0,
        mass = mass,
        invMass = invMass,
        inertia = inertia,
        invInertia = invInertia,
        isStatic = isStatic or false,
        restitution = 0.3,
        staticFriction = 0.6,
        dynamicFriction = 0.4,
        linearDamping = 0.01,
        angularDamping = 0.01,
        gravityScale = 1.0,
        userData = nil
    }
end

local function bodyApplyForce(body, force)
    body.force = vecAdd(body.force, force)
end

local function bodyApplyForceAtPoint(body, force, point)
    body.force = vecAdd(body.force, force)
    local r = vecSub(point, body.position)
    body.torque = body.torque + vecCross(r, force)
end

local function bodyApplyImpulse(body, impulse, contactPoint)
    if body.isStatic then return end
    body.velocity = vecAdd(body.velocity, vecMul(impulse, body.invMass))
    local r = vecSub(contactPoint, body.position)
    body.angularVelocity = body.angularVelocity + body.invInertia * vecCross(r, impulse)
end

local function bodyGetVelocityAtPoint(body, point)
    local r = vecSub(point, body.position)
    return vecAdd(body.velocity, scalarCrossVec(body.angularVelocity, r))
end

local function bodyGetTransformedVertices(body)
    local shape = body.shape
    if shape.type ~= SHAPE_POLYGON then return nil end
    local rot = mat2(body.angle)
    local transformed = {}
    for i = 1, shape.vertexCount do
        local v = mat2MulVec(rot, shape.vertices[i])
        transformed[i] = vecAdd(v, body.position)
    end
    return transformed
end

local function bodyGetTransformedNormals(body)
    local shape = body.shape
    if shape.type ~= SHAPE_POLYGON then return nil end
    local rot = mat2(body.angle)
    local transformed = {}
    for i = 1, shape.vertexCount do
        transformed[i] = mat2MulVec(rot, shape.normals[i])
    end
    return transformed
end

local function bodyGetAABB(body)
    local shape = body.shape
    if shape.type == SHAPE_CIRCLE then
        local r = shape.radius
        return {
            minX = body.position.x - r,
            minY = body.position.y - r,
            maxX = body.position.x + r,
            maxY = body.position.y + r
        }
    else
        local verts = bodyGetTransformedVertices(body)
        local minX, minY = math_huge, math_huge
        local maxX, maxY = -math_huge, -math_huge
        for i = 1, #verts do
            local v = verts[i]
            if v.x < minX then minX = v.x end
            if v.y < minY then minY = v.y end
            if v.x > maxX then maxX = v.x end
            if v.y > maxY then maxY = v.y end
        end
        return {minX = minX, minY = minY, maxX = maxX, maxY = maxY}
    end
end

-- ============================================================================
-- Spatial Hash (broad-phase)
-- ============================================================================

local function createSpatialHash(cellSize)
    return {
        cellSize = cellSize,
        invCellSize = 1 / cellSize,
        cells = {},
        bodyToCells = {}
    }
end

local function spatialHashKey(hash, x, y)
    return x * 73856093 + y * 19349663
end

local function spatialHashClear(hash)
    hash.cells = {}
    hash.bodyToCells = {}
end

local function spatialHashInsert(hash, body)
    local aabb = bodyGetAABB(body)
    local invCell = hash.invCellSize
    local minCX = math_floor(aabb.minX * invCell)
    local minCY = math_floor(aabb.minY * invCell)
    local maxCX = math_floor(aabb.maxX * invCell)
    local maxCY = math_floor(aabb.maxY * invCell)

    local myCells = {}
    for cx = minCX, maxCX do
        for cy = minCY, maxCY do
            local key = spatialHashKey(hash, cx, cy)
            local cell = hash.cells[key]
            if not cell then
                cell = {}
                hash.cells[key] = cell
            end
            cell[#cell + 1] = body
            myCells[#myCells + 1] = key
        end
    end
    hash.bodyToCells[body.id] = myCells
end

local function spatialHashQuery(hash, aabb)
    local invCell = hash.invCellSize
    local minCX = math_floor(aabb.minX * invCell)
    local minCY = math_floor(aabb.minY * invCell)
    local maxCX = math_floor(aabb.maxX * invCell)
    local maxCY = math_floor(aabb.maxY * invCell)

    local seen = {}
    local results = {}
    for cx = minCX, maxCX do
        for cy = minCY, maxCY do
            local key = spatialHashKey(hash, cx, cy)
            local cell = hash.cells[key]
            if cell then
                for i = 1, #cell do
                    local b = cell[i]
                    if not seen[b.id] then
                        seen[b.id] = true
                        results[#results + 1] = b
                    end
                end
            end
        end
    end
    return results
end

local function spatialHashFindPairs(hash, bodies)
    spatialHashClear(hash)
    for i = 1, #bodies do
        spatialHashInsert(hash, bodies[i])
    end

    local foundPairs = {}
    local pairSet = {}

    -- Collect cell keys into an array and sort them for deterministic iteration
    local cellKeys = {}
    for key in next, hash.cells do
        cellKeys[#cellKeys + 1] = key
    end
    table.sort(cellKeys)

    for ki = 1, #cellKeys do
        local cell = hash.cells[cellKeys[ki]]
        local n = #cell
        for i = 1, n do
            for j = i + 1, n do
                local a = cell[i]
                local b = cell[j]
                if not (a.isStatic and b.isStatic) then
                    local pairKey
                    if a.id < b.id then
                        pairKey = a.id * 100000 + b.id
                    else
                        pairKey = b.id * 100000 + a.id
                    end
                    if not pairSet[pairKey] then
                        pairSet[pairKey] = true
                        if a.id < b.id then
                            foundPairs[#foundPairs + 1] = {a = a, b = b}
                        else
                            foundPairs[#foundPairs + 1] = {a = b, b = a}
                        end
                    end
                end
            end
        end
    end
    return foundPairs
end

-- ============================================================================
-- AABB overlap test
-- ============================================================================

local function aabbOverlap(a, b)
    local aabb1 = bodyGetAABB(a)
    local aabb2 = bodyGetAABB(b)
    return aabb1.maxX >= aabb2.minX and aabb1.minX <= aabb2.maxX and
           aabb1.maxY >= aabb2.minY and aabb1.minY <= aabb2.maxY
end

-- ============================================================================
-- Narrow-phase: SAT (Separating Axis Theorem)
-- ============================================================================

local function projectPolygonOnAxis(vertices, axis)
    local min = vecDot(vertices[1], axis)
    local max = min
    for i = 2, #vertices do
        local proj = vecDot(vertices[i], axis)
        if proj < min then min = proj end
        if proj > max then max = proj end
    end
    return min, max
end

local function projectCircleOnAxis(center, radius, axis)
    local proj = vecDot(center, axis)
    return proj - radius, proj + radius
end

local function findPolygonPolygonContacts(bodyA, bodyB)
    local vertsA = bodyGetTransformedVertices(bodyA)
    local vertsB = bodyGetTransformedVertices(bodyB)
    local normalsA = bodyGetTransformedNormals(bodyA)
    local normalsB = bodyGetTransformedNormals(bodyB)

    local minOverlap = math_huge
    local separatingNormal = nil
    local referenceBody = nil
    local incidentBody = nil

    for i = 1, #normalsA do
        local axis = normalsA[i]
        local minA, maxA = projectPolygonOnAxis(vertsA, axis)
        local minB, maxB = projectPolygonOnAxis(vertsB, axis)

        if maxA < minB or maxB < minA then
            return nil
        end

        local overlap = math_min(maxA - minB, maxB - minA)
        if overlap < minOverlap then
            minOverlap = overlap
            separatingNormal = axis
            referenceBody = bodyA
            incidentBody = bodyB
        end
    end

    for i = 1, #normalsB do
        local axis = normalsB[i]
        local minA, maxA = projectPolygonOnAxis(vertsA, axis)
        local minB, maxB = projectPolygonOnAxis(vertsB, axis)

        if maxA < minB or maxB < minA then
            return nil
        end

        local overlap = math_min(maxA - minB, maxB - minA)
        if overlap < minOverlap then
            minOverlap = overlap
            separatingNormal = axis
            referenceBody = bodyB
            incidentBody = bodyA
        end
    end

    local direction = vecSub(bodyB.position, bodyA.position)
    if vecDot(direction, separatingNormal) < 0 then
        separatingNormal = vecNeg(separatingNormal)
    end

    local contacts = findContactPoints_PolygonPolygon(vertsA, vertsB, separatingNormal)

    return {
        bodyA = bodyA,
        bodyB = bodyB,
        normal = separatingNormal,
        penetration = minOverlap,
        contacts = contacts,
        friction = math_sqrt(bodyA.dynamicFriction * bodyB.dynamicFriction),
        restitution = math_max(bodyA.restitution, bodyB.restitution)
    }
end

function findContactPoints_PolygonPolygon(vertsA, vertsB, normal)
    local contacts = {}

    local function findSupport(vertices, direction)
        local maxProj = -math_huge
        local best = nil
        for i = 1, #vertices do
            local proj = vecDot(vertices[i], direction)
            if proj > maxProj then
                maxProj = proj
                best = vertices[i]
            end
        end
        return best
    end

    local function findIncidentEdge(vertices, refNormal)
        local n = #vertices
        local minDot = math_huge
        local edgeIdx = 1
        for i = 1, n do
            local j = (i % n) + 1
            local edge = vecSub(vertices[j], vertices[i])
            local edgeNormal = vecNormalize(vecPerp(edge))
            local d = vecDot(edgeNormal, refNormal)
            if d < minDot then
                minDot = d
                edgeIdx = i
            end
        end
        local j = (edgeIdx % n) + 1
        return vertices[edgeIdx], vertices[j]
    end

    local function clipSegment(v1, v2, normal, offset)
        local out = {}
        local d1 = vecDot(normal, v1) - offset
        local d2 = vecDot(normal, v2) - offset
        if d1 >= 0 then out[#out + 1] = v1 end
        if d2 >= 0 then out[#out + 1] = v2 end
        if d1 * d2 < 0 then
            local t = d1 / (d1 - d2)
            out[#out + 1] = vecLerp(v1, v2, t)
        end
        return out
    end

    local supportA = findSupport(vertsA, normal)
    local supportB = findSupport(vertsB, vecNeg(normal))

    local e1, e2 = findIncidentEdge(vertsB, normal)

    local nA = #vertsA
    local refIdx = 1
    local maxProj = -math_huge
    for i = 1, nA do
        local proj = vecDot(vertsA[i], normal)
        if proj > maxProj then
            maxProj = proj
            refIdx = i
        end
    end

    local refV1 = vertsA[refIdx]
    local refV2 = vertsA[(refIdx % nA) + 1]
    local refEdge = vecNormalize(vecSub(refV2, refV1))
    local refNormal = vecPerp(refEdge)

    local offset1 = vecDot(refEdge, refV1)
    local offset2 = vecDot(refEdge, refV2)

    local clipped = clipSegment(e1, e2, refEdge, offset1)
    if #clipped < 2 then
        contacts[1] = supportB
        return contacts
    end

    clipped = clipSegment(clipped[1], clipped[2], vecNeg(refEdge), -offset2)
    if #clipped < 2 then
        contacts[1] = supportB
        return contacts
    end

    local refOffset = vecDot(refNormal, refV1)
    for i = 1, #clipped do
        local sep = vecDot(refNormal, clipped[i]) - refOffset
        if sep <= 0 then
            contacts[#contacts + 1] = clipped[i]
        end
    end

    if #contacts == 0 then
        contacts[1] = supportB
    end

    return contacts
end

local function findCircleCircleContacts(bodyA, bodyB)
    local diff = vecSub(bodyB.position, bodyA.position)
    local dist = vecLen(diff)
    local radiusSum = bodyA.shape.radius + bodyB.shape.radius

    if dist >= radiusSum then return nil end

    local normal
    if dist < 1e-10 then
        normal = vec(1, 0)
    else
        normal = vecDiv(diff, dist)
    end

    local penetration = radiusSum - dist
    local contactPoint = vecAdd(bodyA.position, vecMul(normal, bodyA.shape.radius - penetration / 2))

    return {
        bodyA = bodyA,
        bodyB = bodyB,
        normal = normal,
        penetration = penetration,
        contacts = {contactPoint},
        friction = math_sqrt(bodyA.dynamicFriction * bodyB.dynamicFriction),
        restitution = math_max(bodyA.restitution, bodyB.restitution)
    }
end

local function findCirclePolygonContacts(circleBody, polyBody)
    local shape = polyBody.shape
    local verts = bodyGetTransformedVertices(polyBody)
    local normals = bodyGetTransformedNormals(polyBody)
    local center = circleBody.position
    local radius = circleBody.shape.radius

    local minOverlap = math_huge
    local separatingNormal = nil
    local axisType = nil

    for i = 1, #normals do
        local axis = normals[i]
        local minP, maxP = projectPolygonOnAxis(verts, axis)
        local minC, maxC = projectCircleOnAxis(center, radius, axis)
        if maxP < minC or maxC < minP then return nil end
        local overlap = math_min(maxP - minC, maxC - minP)
        if overlap < minOverlap then
            minOverlap = overlap
            separatingNormal = axis
            axisType = "face"
        end
    end

    local closestDist = math_huge
    local closestVertex = nil
    for i = 1, #verts do
        local d = vecDistSq(center, verts[i])
        if d < closestDist then
            closestDist = d
            closestVertex = verts[i]
        end
    end

    local vertexAxis = vecNormalize(vecSub(center, closestVertex))
    local minP, maxP = projectPolygonOnAxis(verts, vertexAxis)
    local minC, maxC = projectCircleOnAxis(center, radius, vertexAxis)
    if maxP < minC or maxC < minP then return nil end
    local overlap = math_min(maxP - minC, maxC - minP)
    if overlap < minOverlap then
        minOverlap = overlap
        separatingNormal = vertexAxis
        axisType = "vertex"
    end

    local direction = vecSub(center, polyBody.position)
    if vecDot(direction, separatingNormal) < 0 then
        separatingNormal = vecNeg(separatingNormal)
    end

    local contactPoint = vecSub(center, vecMul(separatingNormal, radius - minOverlap / 2))

    return {
        bodyA = circleBody,
        bodyB = polyBody,
        normal = separatingNormal,
        penetration = minOverlap,
        contacts = {contactPoint},
        friction = math_sqrt(circleBody.dynamicFriction * polyBody.dynamicFriction),
        restitution = math_max(circleBody.restitution, polyBody.restitution)
    }
end

local function detectCollision(bodyA, bodyB)
    local shapeA = bodyA.shape.type
    local shapeB = bodyB.shape.type

    if shapeA == SHAPE_CIRCLE and shapeB == SHAPE_CIRCLE then
        return findCircleCircleContacts(bodyA, bodyB)
    elseif shapeA == SHAPE_POLYGON and shapeB == SHAPE_POLYGON then
        return findPolygonPolygonContacts(bodyA, bodyB)
    elseif shapeA == SHAPE_CIRCLE and shapeB == SHAPE_POLYGON then
        return findCirclePolygonContacts(bodyA, bodyB)
    elseif shapeA == SHAPE_POLYGON and shapeB == SHAPE_CIRCLE then
        local manifold = findCirclePolygonContacts(bodyB, bodyA)
        if manifold then
            manifold.normal = vecNeg(manifold.normal)
            manifold.bodyA = bodyA
            manifold.bodyB = bodyB
        end
        return manifold
    end
    return nil
end

-- ============================================================================
-- Constraint Solver (Sequential Impulses)
-- ============================================================================

local function preSolveContact(manifold, dt)
    local bodyA = manifold.bodyA
    local bodyB = manifold.bodyB
    local normal = manifold.normal
    local tangent = vecPerp(normal)

    manifold.tangent = tangent

    for i = 1, #manifold.contacts do
        local contact = manifold.contacts[i]
        local cp = {}
        cp.point = contact
        cp.rA = vecSub(contact, bodyA.position)
        cp.rB = vecSub(contact, bodyB.position)

        local rnA = vecCross(cp.rA, normal)
        local rnB = vecCross(cp.rB, normal)
        local kNormal = bodyA.invMass + bodyB.invMass +
                        bodyA.invInertia * rnA * rnA +
                        bodyB.invInertia * rnB * rnB
        cp.massNormal = 1 / kNormal

        local rtA = vecCross(cp.rA, tangent)
        local rtB = vecCross(cp.rB, tangent)
        local kTangent = bodyA.invMass + bodyB.invMass +
                         bodyA.invInertia * rtA * rtA +
                         bodyB.invInertia * rtB * rtB
        cp.massTangent = 1 / kTangent

        local relVel = vecSub(
            vecAdd(bodyB.velocity, scalarCrossVec(bodyB.angularVelocity, cp.rB)),
            vecAdd(bodyA.velocity, scalarCrossVec(bodyA.angularVelocity, cp.rA))
        )
        local velAlongNormal = vecDot(relVel, normal)

        cp.bias = 0
        local baumgarte = 0.2
        local slop = 0.005
        if manifold.penetration > slop then
            cp.bias = -baumgarte / dt * (manifold.penetration - slop)
        end

        cp.velocityBias = 0
        if velAlongNormal < -1.0 then
            cp.velocityBias = -manifold.restitution * velAlongNormal
        end

        cp.normalImpulse = 0
        cp.tangentImpulse = 0

        manifold.contacts[i] = cp
    end
end

function solveContact(manifold)
    local bodyA = manifold.bodyA
    local bodyB = manifold.bodyB
    local normal = manifold.normal
    local tangent = manifold.tangent

    for i = 1, #manifold.contacts do
        local cp = manifold.contacts[i]

        local relVel = vecSub(
            vecAdd(bodyB.velocity, scalarCrossVec(bodyB.angularVelocity, cp.rB)),
            vecAdd(bodyA.velocity, scalarCrossVec(bodyA.angularVelocity, cp.rA))
        )

        local velAlongNormal = vecDot(relVel, normal)
        local normalImpulse = cp.massNormal * (-velAlongNormal + cp.bias + cp.velocityBias)

        local oldNormalImpulse = cp.normalImpulse
        cp.normalImpulse = math_max(oldNormalImpulse + normalImpulse, 0)
        normalImpulse = cp.normalImpulse - oldNormalImpulse

        local impulse = vecMul(normal, normalImpulse)
        bodyA.velocity = vecSub(bodyA.velocity, vecMul(impulse, bodyA.invMass))
        bodyA.angularVelocity = bodyA.angularVelocity - bodyA.invInertia * vecCross(cp.rA, impulse)
        bodyB.velocity = vecAdd(bodyB.velocity, vecMul(impulse, bodyB.invMass))
        bodyB.angularVelocity = bodyB.angularVelocity + bodyB.invInertia * vecCross(cp.rB, impulse)

        relVel = vecSub(
            vecAdd(bodyB.velocity, scalarCrossVec(bodyB.angularVelocity, cp.rB)),
            vecAdd(bodyA.velocity, scalarCrossVec(bodyA.angularVelocity, cp.rA))
        )

        local velAlongTangent = vecDot(relVel, tangent)
        local tangentImpulse = cp.massTangent * (-velAlongTangent)

        local maxFriction = manifold.friction * cp.normalImpulse
        local oldTangentImpulse = cp.tangentImpulse
        cp.tangentImpulse = math_max(-maxFriction, math_min(oldTangentImpulse + tangentImpulse, maxFriction))
        tangentImpulse = cp.tangentImpulse - oldTangentImpulse

        local frictionImpulse = vecMul(tangent, tangentImpulse)
        bodyA.velocity = vecSub(bodyA.velocity, vecMul(frictionImpulse, bodyA.invMass))
        bodyA.angularVelocity = bodyA.angularVelocity - bodyA.invInertia * vecCross(cp.rA, frictionImpulse)
        bodyB.velocity = vecAdd(bodyB.velocity, vecMul(frictionImpulse, bodyB.invMass))
        bodyB.angularVelocity = bodyB.angularVelocity + bodyB.invInertia * vecCross(cp.rB, frictionImpulse)
    end
end

-- ============================================================================
-- Joints
-- ============================================================================

local function createDistanceJoint(bodyA, bodyB, anchorA, anchorB, distance)
    return {
        type = "distance",
        bodyA = bodyA,
        bodyB = bodyB,
        localAnchorA = anchorA,
        localAnchorB = anchorB,
        targetDistance = distance,
        stiffness = 100.0,
        damping = 5.0,
        impulse = 0
    }
end

local function createRevoluteJoint(bodyA, bodyB, anchorA, anchorB)
    return {
        type = "revolute",
        bodyA = bodyA,
        bodyB = bodyB,
        localAnchorA = anchorA,
        localAnchorB = anchorB,
        impulse = vec(0, 0),
        motorSpeed = 0,
        maxMotorTorque = 0,
        motorEnabled = false,
        motorImpulse = 0
    }
end

local function createPrismaticJoint(bodyA, bodyB, anchorA, anchorB, axis)
    return {
        type = "prismatic",
        bodyA = bodyA,
        bodyB = bodyB,
        localAnchorA = anchorA,
        localAnchorB = anchorB,
        localAxis = axis,
        impulse = 0,
        motorSpeed = 0,
        maxMotorForce = 0,
        motorEnabled = false
    }
end

function solveDistanceJoint(joint, dt)
    local bodyA = joint.bodyA
    local bodyB = joint.bodyB

    local worldAnchorA = vecAdd(bodyA.position, vecRotate(joint.localAnchorA, bodyA.angle))
    local worldAnchorB = vecAdd(bodyB.position, vecRotate(joint.localAnchorB, bodyB.angle))

    local delta = vecSub(worldAnchorB, worldAnchorA)
    local currentDist = vecLen(delta)
    if currentDist < 1e-10 then return end

    local direction = vecDiv(delta, currentDist)
    local error = currentDist - joint.targetDistance

    local rA = vecSub(worldAnchorA, bodyA.position)
    local rB = vecSub(worldAnchorB, bodyB.position)

    local rnA = vecCross(rA, direction)
    local rnB = vecCross(rB, direction)
    local invEffectiveMass = bodyA.invMass + bodyB.invMass +
                             bodyA.invInertia * rnA * rnA +
                             bodyB.invInertia * rnB * rnB

    local relVel = vecSub(
        vecAdd(bodyB.velocity, scalarCrossVec(bodyB.angularVelocity, rB)),
        vecAdd(bodyA.velocity, scalarCrossVec(bodyA.angularVelocity, rA))
    )
    local velAlongDir = vecDot(relVel, direction)

    local springForce = -joint.stiffness * error
    local dampingForce = -joint.damping * velAlongDir
    local lambda = (springForce + dampingForce) * dt / invEffectiveMass

    local impulse = vecMul(direction, lambda)
    bodyApplyImpulse(bodyA, vecNeg(impulse), worldAnchorA)
    bodyApplyImpulse(bodyB, impulse, worldAnchorB)
end

function solveRevoluteJoint(joint, dt)
    local bodyA = joint.bodyA
    local bodyB = joint.bodyB

    local worldAnchorA = vecAdd(bodyA.position, vecRotate(joint.localAnchorA, bodyA.angle))
    local worldAnchorB = vecAdd(bodyB.position, vecRotate(joint.localAnchorB, bodyB.angle))

    local rA = vecSub(worldAnchorA, bodyA.position)
    local rB = vecSub(worldAnchorB, bodyB.position)

    local error = vecSub(worldAnchorB, worldAnchorA)
    local baumgarte = 0.2
    local correction = vecMul(error, baumgarte / dt)

    local relVel = vecSub(
        vecAdd(bodyB.velocity, scalarCrossVec(bodyB.angularVelocity, rB)),
        vecAdd(bodyA.velocity, scalarCrossVec(bodyA.angularVelocity, rA))
    )

    local Cdot = vecAdd(relVel, correction)

    local k11 = bodyA.invMass + bodyB.invMass +
                bodyA.invInertia * rA.y * rA.y + bodyB.invInertia * rB.y * rB.y
    local k12 = -(bodyA.invInertia * rA.x * rA.y + bodyB.invInertia * rB.x * rB.y)
    local k22 = bodyA.invMass + bodyB.invMass +
                bodyA.invInertia * rA.x * rA.x + bodyB.invInertia * rB.x * rB.x

    local det = k11 * k22 - k12 * k12
    if math_abs(det) < 1e-10 then return end
    local invDet = 1 / det

    local lambda = vec(
        -(k22 * Cdot.x - k12 * Cdot.y) * invDet,
        -(k11 * Cdot.y - k12 * Cdot.x) * invDet
    )

    bodyA.velocity = vecSub(bodyA.velocity, vecMul(lambda, bodyA.invMass))
    bodyA.angularVelocity = bodyA.angularVelocity - bodyA.invInertia * vecCross(rA, lambda)
    bodyB.velocity = vecAdd(bodyB.velocity, vecMul(lambda, bodyB.invMass))
    bodyB.angularVelocity = bodyB.angularVelocity + bodyB.invInertia * vecCross(rB, lambda)

    if joint.motorEnabled then
        local Cdot_motor = bodyB.angularVelocity - bodyA.angularVelocity - joint.motorSpeed
        local motorMass = bodyA.invInertia + bodyB.invInertia
        if motorMass > 0 then
            local motorLambda = -Cdot_motor / motorMass
            local oldImpulse = joint.motorImpulse
            joint.motorImpulse = math_max(-joint.maxMotorTorque * dt,
                                          math_min(oldImpulse + motorLambda, joint.maxMotorTorque * dt))
            motorLambda = joint.motorImpulse - oldImpulse
            bodyA.angularVelocity = bodyA.angularVelocity - bodyA.invInertia * motorLambda
            bodyB.angularVelocity = bodyB.angularVelocity + bodyB.invInertia * motorLambda
        end
    end
end

function solvePrismaticJoint(joint, dt)
    local bodyA = joint.bodyA
    local bodyB = joint.bodyB

    local worldAnchorA = vecAdd(bodyA.position, vecRotate(joint.localAnchorA, bodyA.angle))
    local worldAnchorB = vecAdd(bodyB.position, vecRotate(joint.localAnchorB, bodyB.angle))
    local worldAxis = vecRotate(joint.localAxis, bodyA.angle)
    local perpAxis = vecPerp(worldAxis)

    local rA = vecSub(worldAnchorA, bodyA.position)
    local rB = vecSub(worldAnchorB, bodyB.position)

    local delta = vecSub(worldAnchorB, worldAnchorA)
    local perpError = vecDot(delta, perpAxis)

    local relVel = vecSub(
        vecAdd(bodyB.velocity, scalarCrossVec(bodyB.angularVelocity, rB)),
        vecAdd(bodyA.velocity, scalarCrossVec(bodyA.angularVelocity, rA))
    )
    local perpVel = vecDot(relVel, perpAxis)

    local baumgarte = 0.2
    local bias = baumgarte / dt * perpError

    local rpA = vecCross(rA, perpAxis)
    local rpB = vecCross(rB, perpAxis)
    local effectiveMass = bodyA.invMass + bodyB.invMass +
                          bodyA.invInertia * rpA * rpA +
                          bodyB.invInertia * rpB * rpB

    if effectiveMass < 1e-10 then return end

    local lambda = -(perpVel + bias) / effectiveMass

    local impulse = vecMul(perpAxis, lambda)
    bodyA.velocity = vecSub(bodyA.velocity, vecMul(impulse, bodyA.invMass))
    bodyA.angularVelocity = bodyA.angularVelocity - bodyA.invInertia * vecCross(rA, impulse)
    bodyB.velocity = vecAdd(bodyB.velocity, vecMul(impulse, bodyB.invMass))
    bodyB.angularVelocity = bodyB.angularVelocity + bodyB.invInertia * vecCross(rB, impulse)
end

function solveJoint(joint, dt)
    if joint.type == "distance" then
        solveDistanceJoint(joint, dt)
    elseif joint.type == "revolute" then
        solveRevoluteJoint(joint, dt)
    elseif joint.type == "prismatic" then
        solvePrismaticJoint(joint, dt)
    end
end

-- ============================================================================
-- World
-- ============================================================================

local function createWorld(gravity, cellSize)
    return {
        bodies = {},
        joints = {},
        gravity = gravity or vec(0, -9.81),
        spatialHash = createSpatialHash(cellSize or 2.0),
        manifolds = {},
        iterations = 10,
        dt = 1 / 60
    }
end

local function worldAddBody(world, body)
    world.bodies[#world.bodies + 1] = body
    return body
end

local function worldAddJoint(world, joint)
    world.joints[#world.joints + 1] = joint
    return joint
end

local function worldStep(world, dt)
    dt = dt or world.dt
    local bodies = world.bodies
    local gravity = world.gravity

    for i = 1, #bodies do
        local body = bodies[i]
        if not body.isStatic then
            local gravForce = vecMul(gravity, body.mass * body.gravityScale)
            body.velocity = vecAdd(body.velocity, vecMul(vecAdd(body.force, gravForce), body.invMass * dt))
            body.angularVelocity = body.angularVelocity + body.torque * body.invInertia * dt
            body.velocity = vecMul(body.velocity, 1 / (1 + body.linearDamping * dt))
            body.angularVelocity = body.angularVelocity / (1 + body.angularDamping * dt)
        end
        body.force = vec(0, 0)
        body.torque = 0
    end

    local pairs = spatialHashFindPairs(world.spatialHash, bodies)

    local manifolds = {}
    for i = 1, #pairs do
        local pair = pairs[i]
        if aabbOverlap(pair.a, pair.b) then
            local manifold = detectCollision(pair.a, pair.b)
            if manifold then
                manifolds[#manifolds + 1] = manifold
            end
        end
    end

    for i = 1, #manifolds do
        preSolveContact(manifolds[i], dt)
    end

    for iter = 1, world.iterations do
        for i = 1, #manifolds do
            solveContact(manifolds[i])
        end
        for i = 1, #world.joints do
            solveJoint(world.joints[i], dt)
        end
    end

    for i = 1, #bodies do
        local body = bodies[i]
        if not body.isStatic then
            body.position = vecAdd(body.position, vecMul(body.velocity, dt))
            body.angle = body.angle + body.angularVelocity * dt
        end
    end

    world.manifolds = manifolds
end

-- ============================================================================
-- Ray casting
-- ============================================================================

local function raycastCircle(origin, direction, maxDist, body)
    local center = body.position
    local radius = body.shape.radius
    local oc = vecSub(origin, center)
    local a = vecDot(direction, direction)
    local b = 2 * vecDot(oc, direction)
    local c = vecDot(oc, oc) - radius * radius
    local discriminant = b * b - 4 * a * c
    if discriminant < 0 then return nil end
    local sqrtD = math_sqrt(discriminant)
    local t = (-b - sqrtD) / (2 * a)
    if t < 0 then t = (-b + sqrtD) / (2 * a) end
    if t < 0 or t > maxDist then return nil end
    local point = vecAdd(origin, vecMul(direction, t))
    local normal = vecNormalize(vecSub(point, center))
    return {t = t, point = point, normal = normal, body = body}
end

local function raycastPolygon(origin, direction, maxDist, body)
    local verts = bodyGetTransformedVertices(body)
    local n = #verts
    local tMin = maxDist
    local hitNormal = nil
    local hit = false

    for i = 1, n do
        local j = (i % n) + 1
        local edgeStart = verts[i]
        local edgeEnd = verts[j]
        local edge = vecSub(edgeEnd, edgeStart)
        local denom = direction.x * edge.y - direction.y * edge.x
        if math_abs(denom) > 1e-10 then
            local toStart = vecSub(edgeStart, origin)
            local t = (toStart.x * edge.y - toStart.y * edge.x) / denom
            local u = (toStart.x * direction.y - toStart.y * direction.x) / denom
            if t >= 0 and t < tMin and u >= 0 and u <= 1 then
                tMin = t
                hitNormal = vecNormalize(vecPerp(edge))
                if vecDot(hitNormal, direction) > 0 then
                    hitNormal = vecNeg(hitNormal)
                end
                hit = true
            end
        end
    end

    if not hit then return nil end
    local point = vecAdd(origin, vecMul(direction, tMin))
    return {t = tMin, point = point, normal = hitNormal, body = body}
end

local function worldRaycast(world, origin, direction, maxDist)
    maxDist = maxDist or 1000
    local closest = nil
    for i = 1, #world.bodies do
        local body = world.bodies[i]
        local result
        if body.shape.type == SHAPE_CIRCLE then
            result = raycastCircle(origin, direction, maxDist, body)
        else
            result = raycastPolygon(origin, direction, maxDist, body)
        end
        if result then
            if not closest or result.t < closest.t then
                closest = result
            end
        end
    end
    return closest
end

local function worldRaycastAll(world, origin, direction, maxDist)
    maxDist = maxDist or 1000
    local results = {}
    for i = 1, #world.bodies do
        local body = world.bodies[i]
        local result
        if body.shape.type == SHAPE_CIRCLE then
            result = raycastCircle(origin, direction, maxDist, body)
        else
            result = raycastPolygon(origin, direction, maxDist, body)
        end
        if result then
            results[#results + 1] = result
        end
    end
    table.sort(results, function(a, b) return a.t < b.t end)
    return results
end

-- ============================================================================
-- Continuous Collision Detection (TOI - Time of Impact)
-- ============================================================================

local function computeTOI(bodyA, bodyB, dt)
    local relVel = vecSub(bodyB.velocity, bodyA.velocity)
    local relSpeed = vecLen(relVel)
    if relSpeed < 1e-6 then return 1.0 end

    local maxIterations = 8
    local toi = 1.0
    local tLo = 0
    local tHi = 1.0

    for iter = 1, maxIterations do
        local tMid = (tLo + tHi) / 2
        local posA = vecAdd(bodyA.position, vecMul(bodyA.velocity, tMid * dt))
        local posB = vecAdd(bodyB.position, vecMul(bodyB.velocity, tMid * dt))

        local dist
        if bodyA.shape.type == SHAPE_CIRCLE and bodyB.shape.type == SHAPE_CIRCLE then
            dist = vecDist(posA, posB) - bodyA.shape.radius - bodyB.shape.radius
        else
            dist = 0
            local tempA = {position = posA, angle = bodyA.angle + bodyA.angularVelocity * tMid * dt,
                          shape = bodyA.shape, id = bodyA.id}
            local tempB = {position = posB, angle = bodyB.angle + bodyB.angularVelocity * tMid * dt,
                          shape = bodyB.shape, id = bodyB.id}
            local aabbA = bodyGetAABB(tempA)
            local aabbB = bodyGetAABB(tempB)
            local overlapX = math_min(aabbA.maxX, aabbB.maxX) - math_max(aabbA.minX, aabbB.minX)
            local overlapY = math_min(aabbA.maxY, aabbB.maxY) - math_max(aabbA.minY, aabbB.minY)
            if overlapX > 0 and overlapY > 0 then
                dist = -math_min(overlapX, overlapY)
            else
                dist = math_max(-overlapX, -overlapY)
            end
        end

        if dist < 0.001 then
            tHi = tMid
            toi = tMid
        else
            tLo = tMid
        end

        if tHi - tLo < 0.001 then break end
    end

    return toi
end

-- ============================================================================
-- Island Solver and Sleeping
-- ============================================================================

local SLEEP_TIME_THRESHOLD = 0.5
local SLEEP_LINEAR_THRESHOLD = 0.1
local SLEEP_ANGULAR_THRESHOLD = 0.05

local function bodyCanSleep(body)
    if body.isStatic then return true end
    local linSpeed = vecLen(body.velocity)
    local angSpeed = math_abs(body.angularVelocity)
    return linSpeed < SLEEP_LINEAR_THRESHOLD and angSpeed < SLEEP_ANGULAR_THRESHOLD
end

local function buildIslands(bodies, manifolds)
    local visited = {}
    local islands = {}
    local bodyToManifolds = {}

    for i = 1, #manifolds do
        local m = manifolds[i]
        local idA = m.bodyA.id
        local idB = m.bodyB.id
        if not bodyToManifolds[idA] then bodyToManifolds[idA] = {} end
        if not bodyToManifolds[idB] then bodyToManifolds[idB] = {} end
        bodyToManifolds[idA][#bodyToManifolds[idA] + 1] = m
        bodyToManifolds[idB][#bodyToManifolds[idB] + 1] = m
    end

    for i = 1, #bodies do
        local startBody = bodies[i]
        if not visited[startBody.id] and not startBody.isStatic then
            local island = {bodies = {}, manifolds = {}}
            local stack = {startBody}
            visited[startBody.id] = true

            while #stack > 0 do
                local body = stack[#stack]
                stack[#stack] = nil
                island.bodies[#island.bodies + 1] = body

                local ms = bodyToManifolds[body.id]
                if ms then
                    for j = 1, #ms do
                        local m = ms[j]
                        local seenManifold = false
                        for k = 1, #island.manifolds do
                            if island.manifolds[k] == m then seenManifold = true; break end
                        end
                        if not seenManifold then
                            island.manifolds[#island.manifolds + 1] = m
                        end
                        local other
                        if m.bodyA.id == body.id then other = m.bodyB else other = m.bodyA end
                        if not visited[other.id] and not other.isStatic then
                            visited[other.id] = true
                            stack[#stack + 1] = other
                        end
                    end
                end
            end

            islands[#islands + 1] = island
        end
    end

    return islands
end

-- ============================================================================
-- Weld Joint (locks two bodies together)
-- ============================================================================

local function createWeldJoint(bodyA, bodyB, anchorA, anchorB)
    local referenceAngle = bodyB.angle - bodyA.angle
    return {
        type = "weld",
        bodyA = bodyA,
        bodyB = bodyB,
        localAnchorA = anchorA,
        localAnchorB = anchorB,
        referenceAngle = referenceAngle,
        impulse = vec(0, 0),
        angularImpulse = 0,
        stiffness = 0,
        damping = 0
    }
end

function solveWeldJoint(joint, dt)
    local bodyA = joint.bodyA
    local bodyB = joint.bodyB

    local worldAnchorA = vecAdd(bodyA.position, vecRotate(joint.localAnchorA, bodyA.angle))
    local worldAnchorB = vecAdd(bodyB.position, vecRotate(joint.localAnchorB, bodyB.angle))

    local rA = vecSub(worldAnchorA, bodyA.position)
    local rB = vecSub(worldAnchorB, bodyB.position)

    local posError = vecSub(worldAnchorB, worldAnchorA)
    local angError = bodyB.angle - bodyA.angle - joint.referenceAngle

    local baumgarte = 0.3
    local posCorrection = vecMul(posError, baumgarte / dt)
    local angCorrection = angError * baumgarte / dt

    local relVel = vecSub(
        vecAdd(bodyB.velocity, scalarCrossVec(bodyB.angularVelocity, rB)),
        vecAdd(bodyA.velocity, scalarCrossVec(bodyA.angularVelocity, rA))
    )

    local Cdot = vecAdd(relVel, posCorrection)

    local k11 = bodyA.invMass + bodyB.invMass +
                bodyA.invInertia * rA.y * rA.y + bodyB.invInertia * rB.y * rB.y
    local k12 = -(bodyA.invInertia * rA.x * rA.y + bodyB.invInertia * rB.x * rB.y)
    local k22 = bodyA.invMass + bodyB.invMass +
                bodyA.invInertia * rA.x * rA.x + bodyB.invInertia * rB.x * rB.x

    local det = k11 * k22 - k12 * k12
    if math_abs(det) < 1e-10 then return end
    local invDet = 1 / det

    local lambda = vec(
        -(k22 * Cdot.x - k12 * Cdot.y) * invDet,
        -(k11 * Cdot.y - k12 * Cdot.x) * invDet
    )

    bodyA.velocity = vecSub(bodyA.velocity, vecMul(lambda, bodyA.invMass))
    bodyA.angularVelocity = bodyA.angularVelocity - bodyA.invInertia * vecCross(rA, lambda)
    bodyB.velocity = vecAdd(bodyB.velocity, vecMul(lambda, bodyB.invMass))
    bodyB.angularVelocity = bodyB.angularVelocity + bodyB.invInertia * vecCross(rB, lambda)

    local angMass = bodyA.invInertia + bodyB.invInertia
    if angMass > 0 then
        local relAngVel = bodyB.angularVelocity - bodyA.angularVelocity
        local angLambda = -(relAngVel + angCorrection) / angMass
        bodyA.angularVelocity = bodyA.angularVelocity - bodyA.invInertia * angLambda
        bodyB.angularVelocity = bodyB.angularVelocity + bodyB.invInertia * angLambda
    end
end

-- ============================================================================
-- Rope Joint (max distance constraint)
-- ============================================================================

local function createRopeJoint(bodyA, bodyB, anchorA, anchorB, maxLength)
    return {
        type = "rope",
        bodyA = bodyA,
        bodyB = bodyB,
        localAnchorA = anchorA,
        localAnchorB = anchorB,
        maxLength = maxLength,
        impulse = 0
    }
end

function solveRopeJoint(joint, dt)
    local bodyA = joint.bodyA
    local bodyB = joint.bodyB

    local worldAnchorA = vecAdd(bodyA.position, vecRotate(joint.localAnchorA, bodyA.angle))
    local worldAnchorB = vecAdd(bodyB.position, vecRotate(joint.localAnchorB, bodyB.angle))

    local delta = vecSub(worldAnchorB, worldAnchorA)
    local currentDist = vecLen(delta)
    if currentDist <= joint.maxLength then return end
    if currentDist < 1e-10 then return end

    local direction = vecDiv(delta, currentDist)
    local error = currentDist - joint.maxLength

    local rA = vecSub(worldAnchorA, bodyA.position)
    local rB = vecSub(worldAnchorB, bodyB.position)

    local rnA = vecCross(rA, direction)
    local rnB = vecCross(rB, direction)
    local invEffectiveMass = bodyA.invMass + bodyB.invMass +
                             bodyA.invInertia * rnA * rnA +
                             bodyB.invInertia * rnB * rnB

    if invEffectiveMass < 1e-10 then return end

    local relVel = vecSub(
        vecAdd(bodyB.velocity, scalarCrossVec(bodyB.angularVelocity, rB)),
        vecAdd(bodyA.velocity, scalarCrossVec(bodyA.angularVelocity, rA))
    )
    local velAlongDir = vecDot(relVel, direction)

    local baumgarte = 0.3
    local bias = baumgarte / dt * error
    local lambda = -(velAlongDir + bias) / invEffectiveMass

    local oldImpulse = joint.impulse
    joint.impulse = math_max(0, oldImpulse + lambda)
    lambda = joint.impulse - oldImpulse

    local impulse = vecMul(direction, lambda)
    bodyApplyImpulse(bodyA, vecNeg(impulse), worldAnchorA)
    bodyApplyImpulse(bodyB, impulse, worldAnchorB)
end

-- ============================================================================
-- Wheel Joint (spring + revolute, for vehicles)
-- ============================================================================

local function createWheelJoint(bodyA, bodyB, anchorA, anchorB, axis)
    return {
        type = "wheel",
        bodyA = bodyA,
        bodyB = bodyB,
        localAnchorA = anchorA,
        localAnchorB = anchorB,
        localAxis = axis,
        springStiffness = 50.0,
        springDamping = 5.0,
        motorSpeed = 0,
        maxMotorTorque = 0,
        motorEnabled = false,
        springImpulse = 0,
        motorImpulse = 0
    }
end

function solveWheelJoint(joint, dt)
    local bodyA = joint.bodyA
    local bodyB = joint.bodyB

    local worldAnchorA = vecAdd(bodyA.position, vecRotate(joint.localAnchorA, bodyA.angle))
    local worldAnchorB = vecAdd(bodyB.position, vecRotate(joint.localAnchorB, bodyB.angle))
    local worldAxis = vecRotate(joint.localAxis, bodyA.angle)
    local perpAxis = vecPerp(worldAxis)

    local rA = vecSub(worldAnchorA, bodyA.position)
    local rB = vecSub(worldAnchorB, bodyB.position)

    local delta = vecSub(worldAnchorB, worldAnchorA)
    local springError = vecDot(delta, worldAxis)

    local relVel = vecSub(
        vecAdd(bodyB.velocity, scalarCrossVec(bodyB.angularVelocity, rB)),
        vecAdd(bodyA.velocity, scalarCrossVec(bodyA.angularVelocity, rA))
    )
    local springVel = vecDot(relVel, worldAxis)

    local raAxis = vecCross(rA, worldAxis)
    local rbAxis = vecCross(rB, worldAxis)
    local springMass = bodyA.invMass + bodyB.invMass +
                       bodyA.invInertia * raAxis * raAxis +
                       bodyB.invInertia * rbAxis * rbAxis

    if springMass > 1e-10 then
        local springForce = -joint.springStiffness * springError - joint.springDamping * springVel
        local lambda = springForce * dt / springMass
        local impulse = vecMul(worldAxis, lambda)
        bodyA.velocity = vecSub(bodyA.velocity, vecMul(impulse, bodyA.invMass))
        bodyA.angularVelocity = bodyA.angularVelocity - bodyA.invInertia * vecCross(rA, impulse)
        bodyB.velocity = vecAdd(bodyB.velocity, vecMul(impulse, bodyB.invMass))
        bodyB.angularVelocity = bodyB.angularVelocity + bodyB.invInertia * vecCross(rB, impulse)
    end

    local perpError = vecDot(delta, perpAxis)
    local perpVel = vecDot(relVel, perpAxis)
    local raPerp = vecCross(rA, perpAxis)
    local rbPerp = vecCross(rB, perpAxis)
    local perpMass = bodyA.invMass + bodyB.invMass +
                     bodyA.invInertia * raPerp * raPerp +
                     bodyB.invInertia * rbPerp * rbPerp

    if perpMass > 1e-10 then
        local bias = 0.2 / dt * perpError
        local lambda = -(perpVel + bias) / perpMass
        local impulse = vecMul(perpAxis, lambda)
        bodyA.velocity = vecSub(bodyA.velocity, vecMul(impulse, bodyA.invMass))
        bodyA.angularVelocity = bodyA.angularVelocity - bodyA.invInertia * vecCross(rA, impulse)
        bodyB.velocity = vecAdd(bodyB.velocity, vecMul(impulse, bodyB.invMass))
        bodyB.angularVelocity = bodyB.angularVelocity + bodyB.invInertia * vecCross(rB, impulse)
    end

    if joint.motorEnabled then
        local motorMass = bodyA.invInertia + bodyB.invInertia
        if motorMass > 0 then
            local Cdot = bodyB.angularVelocity - bodyA.angularVelocity - joint.motorSpeed
            local motorLambda = -Cdot / motorMass
            local oldImpulse = joint.motorImpulse
            joint.motorImpulse = math_max(-joint.maxMotorTorque * dt,
                                          math_min(oldImpulse + motorLambda, joint.maxMotorTorque * dt))
            motorLambda = joint.motorImpulse - oldImpulse
            bodyA.angularVelocity = bodyA.angularVelocity - bodyA.invInertia * motorLambda
            bodyB.angularVelocity = bodyB.angularVelocity + bodyB.invInertia * motorLambda
        end
    end
end

-- ============================================================================
-- Gear Joint (couples two revolute joints)
-- ============================================================================

local function createGearJoint(jointA, jointB, ratio)
    return {
        type = "gear",
        jointA = jointA,
        jointB = jointB,
        bodyA = jointA.bodyB,
        bodyB = jointB.bodyB,
        bodyGround = jointA.bodyA,
        ratio = ratio,
        impulse = 0
    }
end

function solveGearJoint(joint, dt)
    local bodyA = joint.bodyA
    local bodyB = joint.bodyB
    local ratio = joint.ratio

    local angVelA = bodyA.angularVelocity
    local angVelB = bodyB.angularVelocity
    local Cdot = angVelA + ratio * angVelB

    local mass = bodyA.invInertia + ratio * ratio * bodyB.invInertia
    if mass < 1e-10 then return end

    local lambda = -Cdot / mass
    joint.impulse = joint.impulse + lambda

    bodyA.angularVelocity = bodyA.angularVelocity + bodyA.invInertia * lambda
    bodyB.angularVelocity = bodyB.angularVelocity + bodyB.invInertia * lambda * ratio
end

-- ============================================================================
-- Convex Hull computation (Andrew's monotone chain)
-- ============================================================================

local function computeConvexHull(points)
    local n = #points
    if n < 3 then return points end

    table.sort(points, function(a, b)
        if a.x == b.x then return a.y < b.y end
        return a.x < b.x
    end)

    local hull = {}
    local k = 0

    for i = 1, n do
        while k >= 2 and vecCross(vecSub(hull[k], hull[k-1]), vecSub(points[i], hull[k-1])) <= 0 do
            k = k - 1
        end
        k = k + 1
        hull[k] = points[i]
    end

    local lower = k + 1
    for i = n - 1, 1, -1 do
        while k >= lower and vecCross(vecSub(hull[k], hull[k-1]), vecSub(points[i], hull[k-1])) <= 0 do
            k = k - 1
        end
        k = k + 1
        hull[k] = points[i]
    end

    local result = {}
    for i = 1, k - 1 do
        result[i] = hull[i]
    end
    return result
end

-- ============================================================================
-- Minkowski Difference support (for GJK-like queries)
-- ============================================================================

local function support(shape, position, angle, direction)
    if shape.type == SHAPE_CIRCLE then
        local norm = vecNormalize(direction)
        return vecAdd(position, vecMul(norm, shape.radius))
    else
        local rot = mat2(angle)
        local invRot = mat2Transpose(rot)
        local localDir = mat2MulVec(invRot, direction)
        local best = shape.vertices[1]
        local bestDot = vecDot(best, localDir)
        for i = 2, shape.vertexCount do
            local d = vecDot(shape.vertices[i], localDir)
            if d > bestDot then
                bestDot = d
                best = shape.vertices[i]
            end
        end
        return vecAdd(mat2MulVec(rot, best), position)
    end
end

local function minkowskiSupport(bodyA, bodyB, direction)
    local pointA = support(bodyA.shape, bodyA.position, bodyA.angle, direction)
    local pointB = support(bodyB.shape, bodyB.position, bodyB.angle, vecNeg(direction))
    return vecSub(pointA, pointB)
end

-- ============================================================================
-- Point-in-shape queries
-- ============================================================================

local function pointInCircle(point, body)
    local dist = vecDist(point, body.position)
    return dist <= body.shape.radius
end

local function pointInPolygon(point, body)
    local verts = bodyGetTransformedVertices(body)
    local n = #verts
    for i = 1, n do
        local j = (i % n) + 1
        local edge = vecSub(verts[j], verts[i])
        local toPoint = vecSub(point, verts[i])
        if vecCross(edge, toPoint) < 0 then
            return false
        end
    end
    return true
end

local function pointInBody(point, body)
    if body.shape.type == SHAPE_CIRCLE then
        return pointInCircle(point, body)
    else
        return pointInPolygon(point, body)
    end
end

local function worldQueryPoint(world, point)
    local results = {}
    for i = 1, #world.bodies do
        if pointInBody(point, world.bodies[i]) then
            results[#results + 1] = world.bodies[i]
        end
    end
    return results
end

-- ============================================================================
-- AABB query
-- ============================================================================

local function worldQueryAABB(world, queryAABB)
    local results = {}
    for i = 1, #world.bodies do
        local bodyAABB = bodyGetAABB(world.bodies[i])
        if bodyAABB.maxX >= queryAABB.minX and bodyAABB.minX <= queryAABB.maxX and
           bodyAABB.maxY >= queryAABB.minY and bodyAABB.minY <= queryAABB.maxY then
            results[#results + 1] = world.bodies[i]
        end
    end
    return results
end

-- ============================================================================
-- Distance computation between shapes
-- ============================================================================

local function closestPointOnSegment(point, segStart, segEnd)
    local seg = vecSub(segEnd, segStart)
    local t = vecDot(vecSub(point, segStart), seg) / vecDot(seg, seg)
    t = math_max(0, math_min(1, t))
    return vecAdd(segStart, vecMul(seg, t))
end

local function distancePointToPolygon(point, body)
    local verts = bodyGetTransformedVertices(body)
    local n = #verts
    local minDist = math_huge
    for i = 1, n do
        local j = (i % n) + 1
        local closest = closestPointOnSegment(point, verts[i], verts[j])
        local dist = vecDist(point, closest)
        if dist < minDist then minDist = dist end
    end
    return minDist
end

local function distanceBetweenBodies(bodyA, bodyB)
    if bodyA.shape.type == SHAPE_CIRCLE and bodyB.shape.type == SHAPE_CIRCLE then
        local d = vecDist(bodyA.position, bodyB.position) - bodyA.shape.radius - bodyB.shape.radius
        return math_max(0, d)
    elseif bodyA.shape.type == SHAPE_CIRCLE then
        local d = distancePointToPolygon(bodyA.position, bodyB) - bodyA.shape.radius
        return math_max(0, d)
    elseif bodyB.shape.type == SHAPE_CIRCLE then
        local d = distancePointToPolygon(bodyB.position, bodyA) - bodyB.shape.radius
        return math_max(0, d)
    else
        local vertsA = bodyGetTransformedVertices(bodyA)
        local vertsB = bodyGetTransformedVertices(bodyB)
        local minDist = math_huge
        for i = 1, #vertsA do
            for j = 1, #vertsB do
                local nB = #vertsB
                local j2 = (j % nB) + 1
                local closest = closestPointOnSegment(vertsA[i], vertsB[j], vertsB[j2])
                local d = vecDist(vertsA[i], closest)
                if d < minDist then minDist = d end
            end
        end
        for i = 1, #vertsB do
            for j = 1, #vertsA do
                local nA = #vertsA
                local j2 = (j % nA) + 1
                local closest = closestPointOnSegment(vertsB[i], vertsA[j], vertsA[j2])
                local d = vecDist(vertsB[i], closest)
                if d < minDist then minDist = d end
            end
        end
        return minDist
    end
end

-- ============================================================================
-- Extended World step with joints
-- ============================================================================

function solveJointExtended(joint, dt)
    if joint.type == "distance" then
        solveDistanceJoint(joint, dt)
    elseif joint.type == "revolute" then
        solveRevoluteJoint(joint, dt)
    elseif joint.type == "prismatic" then
        solvePrismaticJoint(joint, dt)
    elseif joint.type == "weld" then
        solveWeldJoint(joint, dt)
    elseif joint.type == "rope" then
        solveRopeJoint(joint, dt)
    elseif joint.type == "wheel" then
        solveWheelJoint(joint, dt)
    elseif joint.type == "gear" then
        solveGearJoint(joint, dt)
    end
end

local function worldStepExtended(world, dt)
    dt = dt or world.dt
    local bodies = world.bodies
    local gravity = world.gravity

    for i = 1, #bodies do
        local body = bodies[i]
        if not body.isStatic then
            local gravForce = vecMul(gravity, body.mass * body.gravityScale)
            body.velocity = vecAdd(body.velocity, vecMul(vecAdd(body.force, gravForce), body.invMass * dt))
            body.angularVelocity = body.angularVelocity + body.torque * body.invInertia * dt
            body.velocity = vecMul(body.velocity, 1 / (1 + body.linearDamping * dt))
            body.angularVelocity = body.angularVelocity / (1 + body.angularDamping * dt)
        end
        body.force = vec(0, 0)
        body.torque = 0
    end

    local bpPairs = spatialHashFindPairs(world.spatialHash, bodies)

    local manifolds = {}
    for i = 1, #bpPairs do
        local pair = bpPairs[i]
        if aabbOverlap(pair.a, pair.b) then
            local manifold = detectCollision(pair.a, pair.b)
            if manifold then
                manifolds[#manifolds + 1] = manifold
            end
        end
    end

    for i = 1, #manifolds do
        preSolveContact(manifolds[i], dt)
    end

    for iter = 1, world.iterations do
        for i = 1, #manifolds do
            solveContact(manifolds[i])
        end
        for i = 1, #world.joints do
            solveJointExtended(world.joints[i], dt)
        end
    end

    for i = 1, #bodies do
        local body = bodies[i]
        if not body.isStatic then
            body.position = vecAdd(body.position, vecMul(body.velocity, dt))
            body.angle = body.angle + body.angularVelocity * dt
        end
    end

    world.manifolds = manifolds
end

-- ============================================================================
-- Scenario 1: Box Stack (tests resting contacts and friction)
-- ============================================================================

function createBoxStackScenario()
    local world = createWorld(vec(0, -20), 3.0)

    local ground = createBody(createBox(50, 1), 0, -1, 1, true)
    ground.restitution = 0.0
    worldAddBody(world, ground)

    local wallLeft = createBody(createBox(1, 30), -15, 15, 1, true)
    worldAddBody(world, wallLeft)
    local wallRight = createBody(createBox(1, 30), 15, 15, 1, true)
    worldAddBody(world, wallRight)

    for row = 0, 9 do
        local numBoxes = 10 - row
        local startX = -(numBoxes - 1) * 1.1 / 2
        for col = 0, numBoxes - 1 do
            local x = startX + col * 1.1
            local y = 0.5 + row * 1.05
            local box = createBody(createBox(0.5, 0.5), x, y, 2.0, false)
            box.restitution = 0.0
            box.staticFriction = 0.7
            box.dynamicFriction = 0.5
            worldAddBody(world, box)
        end
    end

    return world
end

-- ============================================================================
-- Scenario 2: Pendulum Chain (tests revolute joints)
-- ============================================================================

function createPendulumScenario()
    local world = createWorld(vec(0, -10), 4.0)

    local anchor = createBody(createCircle(0.3), 0, 15, 1, true)
    worldAddBody(world, anchor)

    local numLinks = 12
    local linkLength = 1.5
    local prevBody = anchor

    for i = 1, numLinks do
        local x = i * linkLength
        local y = 15
        local link = createBody(createBox(0.6, 0.2), x, y, 3.0, false)
        link.restitution = 0.1
        link.angularDamping = 0.05
        worldAddBody(world, link)

        local jointAnchorA = vec(0.3, 0)
        local jointAnchorB = vec(-0.3, 0)
        if i == 1 then
            jointAnchorA = vec(0, 0)
        end
        local joint = createRevoluteJoint(prevBody, link, jointAnchorA, jointAnchorB)
        worldAddJoint(world, joint)

        prevBody = link
    end

    local ball = createBody(createCircle(1.0), numLinks * linkLength + 1.5, 15, 5.0, false)
    ball.restitution = 0.5
    worldAddBody(world, ball)
    local lastJoint = createRevoluteJoint(prevBody, ball, vec(0.3, 0), vec(-0.5, 0))
    worldAddJoint(world, lastJoint)

    for i = 1, numLinks + 2 do
        local body = world.bodies[i + 1]
        if body and not body.isStatic then
            body.velocity = vec(0, -5)
        end
    end

    return world
end

-- ============================================================================
-- Scenario 3: Ball Pit (tests broad-phase with many circles)
-- ============================================================================

function createBallPitScenario()
    local world = createWorld(vec(0, -15), 2.0)

    local floor = createBody(createBox(20, 1), 0, -1, 1, true)
    floor.restitution = 0.4
    worldAddBody(world, floor)

    local leftWall = createBody(createBox(1, 15), -11, 7, 1, true)
    leftWall.restitution = 0.4
    worldAddBody(world, leftWall)
    local rightWall = createBody(createBox(1, 15), 11, 7, 1, true)
    rightWall.restitution = 0.4
    worldAddBody(world, rightWall)

    local rampShape = createPolygon({
        vec(-5, -0.5), vec(5, 0.5), vec(5, -0.5)
    })
    local ramp = createBody(rampShape, -3, 10, 1, true)
    worldAddBody(world, ramp)
    local ramp2Shape = createPolygon({
        vec(-5, 0.5), vec(5, -0.5), vec(-5, -0.5)
    })
    local ramp2 = createBody(ramp2Shape, 3, 6, 1, true)
    worldAddBody(world, ramp2)

    resetRandom()
    for i = 1, 80 do
        local radius = randomRange(0.3, 0.8)
        local x = randomRange(-8, 8)
        local y = randomRange(12, 30)
        local ball = createBody(createCircle(radius), x, y, 1.5, false)
        ball.restitution = randomRange(0.3, 0.8)
        ball.dynamicFriction = randomRange(0.2, 0.5)
        worldAddBody(world, ball)
    end

    return world
end

-- ============================================================================
-- Scenario 4: Domino Chain (tests sequential collisions)
-- ============================================================================

function createDominoScenario()
    local world = createWorld(vec(0, -10), 2.5)

    local ground = createBody(createBox(40, 1), 0, -1, 1, true)
    ground.restitution = 0.0
    ground.staticFriction = 0.8
    worldAddBody(world, ground)

    local numDominoes = 25
    local spacing = 1.2
    local startX = -(numDominoes * spacing) / 2

    for i = 0, numDominoes - 1 do
        local x = startX + i * spacing
        local domino = createBody(createBox(0.15, 1.0), x, 1.0, 4.0, false)
        domino.restitution = 0.0
        domino.staticFriction = 0.6
        domino.dynamicFriction = 0.4
        worldAddBody(world, domino)
    end

    local pusher = createBody(createCircle(0.5), startX - 1.5, 1.5, 10.0, false)
    pusher.velocity = vec(8, 0)
    pusher.restitution = 0.0
    worldAddBody(world, pusher)

    local rampX = startX + numDominoes * spacing + 2
    local rampVerts = {
        vec(-2, 0), vec(2, 2), vec(2, 0)
    }
    local rampBody = createBody(createPolygon(rampVerts), rampX, 0, 1, true)
    worldAddBody(world, rampBody)

    return world
end

-- ============================================================================
-- Scenario 5: Billiards (tests circle-circle collisions and rebounds)
-- ============================================================================

function createBilliardsScenario()
    local world = createWorld(vec(0, 0), 3.0)
    world.gravity = vec(0, 0)

    local tableW = 20
    local tableH = 10
    local cushionThickness = 0.5

    local topCushion = createBody(createBox(tableW / 2 + cushionThickness, cushionThickness),
                                   0, tableH / 2 + cushionThickness, 1, true)
    topCushion.restitution = 0.85
    worldAddBody(world, topCushion)

    local bottomCushion = createBody(createBox(tableW / 2 + cushionThickness, cushionThickness),
                                      0, -tableH / 2 - cushionThickness, 1, true)
    bottomCushion.restitution = 0.85
    worldAddBody(world, bottomCushion)

    local leftCushion = createBody(createBox(cushionThickness, tableH / 2 + cushionThickness),
                                    -tableW / 2 - cushionThickness, 0, 1, true)
    leftCushion.restitution = 0.85
    worldAddBody(world, leftCushion)

    local rightCushion = createBody(createBox(cushionThickness, tableH / 2 + cushionThickness),
                                     tableW / 2 + cushionThickness, 0, 1, true)
    rightCushion.restitution = 0.85
    worldAddBody(world, rightCushion)

    local ballRadius = 0.4
    local ballDensity = 2.0

    local cueBall = createBody(createCircle(ballRadius), -6, 0, ballDensity, false)
    cueBall.restitution = 0.95
    cueBall.linearDamping = 0.3
    cueBall.dynamicFriction = 0.1
    cueBall.velocity = vec(15, 0.5)
    worldAddBody(world, cueBall)

    local rackX = 4
    local rackY = 0
    local ballSpacing = ballRadius * 2.05
    local row = 0
    local col = 0
    local ballCount = 0
    for r = 0, 4 do
        for c = 0, r do
            local x = rackX + r * ballSpacing * 0.866
            local y = rackY + (c - r / 2) * ballSpacing
            local ball = createBody(createCircle(ballRadius), x, y, ballDensity, false)
            ball.restitution = 0.95
            ball.linearDamping = 0.3
            ball.dynamicFriction = 0.1
            worldAddBody(world, ball)
            ballCount = ballCount + 1
        end
    end

    return world
end

-- ============================================================================
-- Scenario 6: Mixed Shapes Tumbler (polygon variety + rotation)
-- ============================================================================

function createTumblerScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local containerSize = 8
    local wallThickness = 0.3

    local bottom = createBody(createBox(containerSize, wallThickness), 0, -containerSize, 1, true)
    worldAddBody(world, bottom)
    local top = createBody(createBox(containerSize, wallThickness), 0, containerSize, 1, true)
    worldAddBody(world, top)
    local left = createBody(createBox(wallThickness, containerSize), -containerSize, 0, 1, true)
    worldAddBody(world, left)
    local right = createBody(createBox(wallThickness, containerSize), containerSize, 0, 1, true)
    worldAddBody(world, right)

    resetRandom()
    local shapes = {}
    for i = 1, 40 do
        local shapeType = math_floor(random() * 4)
        local x = randomRange(-6, 6)
        local y = randomRange(-4, 6)
        local body

        if shapeType == 0 then
            body = createBody(createCircle(randomRange(0.3, 0.7)), x, y, 2.0, false)
        elseif shapeType == 1 then
            local hw = randomRange(0.3, 0.8)
            local hh = randomRange(0.3, 0.8)
            body = createBody(createBox(hw, hh), x, y, 2.0, false)
        elseif shapeType == 2 then
            body = createBody(createRegularPolygon(randomRange(0.4, 0.7), 5), x, y, 2.0, false)
        else
            body = createBody(createRegularPolygon(randomRange(0.4, 0.7), 6), x, y, 2.0, false)
        end

        body.angle = randomRange(0, math_pi * 2)
        body.restitution = randomRange(0.1, 0.5)
        body.dynamicFriction = randomRange(0.3, 0.6)
        worldAddBody(world, body)
    end

    return world
end

-- ============================================================================
-- Scenario 7: Bridge with distance joints
-- ============================================================================

function createBridgeScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local numSegments = 15
    local segmentWidth = 1.2
    local segmentHeight = 0.2
    local bridgeY = 8
    local bridgeStartX = -(numSegments * segmentWidth) / 2

    local leftAnchor = createBody(createBox(1, 1), bridgeStartX - 1.5, bridgeY, 1, true)
    worldAddBody(world, leftAnchor)
    local rightAnchor = createBody(createBox(1, 1), bridgeStartX + numSegments * segmentWidth + 1.5, bridgeY, 1, true)
    worldAddBody(world, rightAnchor)

    local prevBody = leftAnchor
    local segments = {}
    for i = 1, numSegments do
        local x = bridgeStartX + (i - 0.5) * segmentWidth
        local seg = createBody(createBox(segmentWidth / 2 - 0.05, segmentHeight), x, bridgeY, 3.0, false)
        seg.linearDamping = 0.1
        seg.angularDamping = 0.2
        worldAddBody(world, seg)
        segments[i] = seg

        local joint = createDistanceJoint(prevBody, seg,
            vec(segmentWidth / 2, 0), vec(-segmentWidth / 2 + 0.05, 0),
            0.1)
        joint.stiffness = 200
        joint.damping = 10
        worldAddJoint(world, joint)
        prevBody = seg
    end

    local lastJoint = createDistanceJoint(prevBody, rightAnchor,
        vec(segmentWidth / 2, 0), vec(-1, 0), 0.1)
    lastJoint.stiffness = 200
    lastJoint.damping = 10
    worldAddJoint(world, lastJoint)

    local heavyBall = createBody(createCircle(0.8), 0, bridgeY + 5, 8.0, false)
    heavyBall.restitution = 0.2
    worldAddBody(world, heavyBall)

    local ground = createBody(createBox(30, 1), 0, -1, 1, true)
    worldAddBody(world, ground)

    return world
end

-- ============================================================================
-- Scenario 8: Newton's Cradle (tests energy transfer)
-- ============================================================================

function createCradleScenario()
    local world = createWorld(vec(0, -10), 2.0)

    local numBalls = 7
    local ballRadius = 0.5
    local stringLength = 6
    local spacing = ballRadius * 2.01
    local anchorY = 12
    local startX = -(numBalls - 1) * spacing / 2

    for i = 0, numBalls - 1 do
        local x = startX + i * spacing
        local ballY = anchorY - stringLength

        local anchor = createBody(createCircle(0.1), x, anchorY, 1, true)
        worldAddBody(world, anchor)

        local ball = createBody(createCircle(ballRadius), x, ballY, 8.0, false)
        ball.restitution = 0.99
        ball.linearDamping = 0.001
        ball.dynamicFriction = 0.01
        worldAddBody(world, ball)

        local joint = createDistanceJoint(anchor, ball, vec(0, 0), vec(0, 0), stringLength)
        joint.stiffness = 500
        joint.damping = 2
        worldAddJoint(world, joint)
    end

    local firstBall = world.bodies[3]
    firstBall.position = vec(startX - 3, anchorY - stringLength + 3)
    firstBall.velocity = vec(5, -3)

    return world
end

-- ============================================================================
-- Scenario 9: Vehicle on terrain (wheel joints + uneven ground)
-- ============================================================================

function createVehicleScenario()
    local world = createWorld(vec(0, -10), 4.0)

    local terrainPoints = {}
    local terrainSegments = 40
    local terrainWidth = 60
    local segWidth = terrainWidth / terrainSegments
    resetRandom()

    local height = 0
    for i = 0, terrainSegments do
        height = height + randomRange(-0.5, 0.5)
        if height < -3 then height = -3 end
        if height > 3 then height = 3 end
        terrainPoints[i + 1] = vec(-terrainWidth / 2 + i * segWidth, height)
    end

    for i = 1, terrainSegments do
        local p1 = terrainPoints[i]
        local p2 = terrainPoints[i + 1]
        local midX = (p1.x + p2.x) / 2
        local midY = (p1.y + p2.y) / 2
        local dx = p2.x - p1.x
        local dy = p2.y - p1.y
        local len = math_sqrt(dx * dx + dy * dy)
        local angle = math_atan2(dy, dx)

        local seg = createBody(createBox(len / 2, 0.3), midX, midY - 0.3, 1, true)
        seg.angle = angle
        seg.restitution = 0.1
        seg.staticFriction = 0.9
        worldAddBody(world, seg)
    end

    local chassisW = 2.5
    local chassisH = 0.5
    local chassis = createBody(createBox(chassisW, chassisH), -20, 4, 3.0, false)
    chassis.linearDamping = 0.05
    worldAddBody(world, chassis)

    local wheelRadius = 0.6
    local wheelDensity = 2.0
    local frontWheel = createBody(createCircle(wheelRadius), -20 + chassisW - 0.3, 3, wheelDensity, false)
    frontWheel.dynamicFriction = 0.9
    frontWheel.restitution = 0.1
    worldAddBody(world, frontWheel)

    local rearWheel = createBody(createCircle(wheelRadius), -20 - chassisW + 0.3, 3, wheelDensity, false)
    rearWheel.dynamicFriction = 0.9
    rearWheel.restitution = 0.1
    worldAddBody(world, rearWheel)

    local frontJoint = createWheelJoint(chassis, frontWheel,
        vec(chassisW - 0.3, -chassisH), vec(0, 0), vec(0, 1))
    frontJoint.springStiffness = 80
    frontJoint.springDamping = 8
    worldAddJoint(world, frontJoint)

    local rearJoint = createWheelJoint(chassis, rearWheel,
        vec(-chassisW + 0.3, -chassisH), vec(0, 0), vec(0, 1))
    rearJoint.springStiffness = 80
    rearJoint.springDamping = 8
    rearJoint.motorEnabled = true
    rearJoint.motorSpeed = -15
    rearJoint.maxMotorTorque = 50
    worldAddJoint(world, rearJoint)

    return world
end

-- ============================================================================
-- Scenario 10: Wrecking ball (rope joint + heavy ball + structure)
-- ============================================================================

function createWreckingBallScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(30, 1), 0, -1, 1, true)
    worldAddBody(world, ground)

    local towerX = 5
    local brickW = 0.8
    local brickH = 0.4
    for row = 0, 7 do
        local numBricks = 4
        for col = 0, numBricks - 1 do
            local x = towerX + (col - (numBricks - 1) / 2) * (brickW * 2 + 0.05)
            local y = 0.4 + row * (brickH * 2 + 0.02)
            local brick = createBody(createBox(brickW, brickH), x, y, 2.0, false)
            brick.restitution = 0.0
            brick.staticFriction = 0.6
            worldAddBody(world, brick)
        end
    end

    local craneX = -10
    local craneY = 15
    local anchor = createBody(createCircle(0.2), craneX, craneY, 1, true)
    worldAddBody(world, anchor)

    local ropeLength = 12
    local numRopeLinks = 8
    local linkLen = ropeLength / numRopeLinks
    local prevBody = anchor
    for i = 1, numRopeLinks do
        local x = craneX
        local y = craneY - i * linkLen
        local link = createBody(createBox(0.15, linkLen / 2 - 0.05), x, y, 1.0, false)
        link.angularDamping = 0.1
        worldAddBody(world, link)

        local joint = createRevoluteJoint(prevBody, link,
            vec(0, i == 1 and 0 or -linkLen / 2 + 0.05),
            vec(0, linkLen / 2 - 0.05))
        worldAddJoint(world, joint)
        prevBody = link
    end

    local ballRadius = 1.2
    local ball = createBody(createCircle(ballRadius), craneX, craneY - ropeLength - ballRadius, 15.0, false)
    ball.restitution = 0.1
    worldAddBody(world, ball)

    local ballJoint = createRevoluteJoint(prevBody, ball, vec(0, -linkLen / 2), vec(0, 0))
    worldAddJoint(world, ballJoint)

    ball.velocity = vec(12, 5)

    return world
end

-- ============================================================================
-- Scenario 11: Gear train (coupled revolute joints)
-- ============================================================================

function createGearTrainScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(20, 1), 0, -1, 1, true)
    worldAddBody(world, ground)

    local gearData = {
        {x = 0, y = 5, radius = 1.0, sides = 12, density = 3.0},
        {x = 2.2, y = 5, radius = 0.7, sides = 9, density = 3.0},
        {x = 3.9, y = 5, radius = 1.2, sides = 14, density = 3.0},
        {x = 6.3, y = 5, radius = 0.5, sides = 8, density = 3.0},
        {x = 7.5, y = 5, radius = 0.9, sides = 11, density = 3.0},
    }

    local gearBodies = {}
    local gearJoints = {}

    for i = 1, #gearData do
        local gd = gearData[i]
        local gear = createBody(createRegularPolygon(gd.radius, gd.sides), gd.x, gd.y, gd.density, false)
        gear.angularDamping = 0.02
        worldAddBody(world, gear)
        gearBodies[i] = gear

        local pivot = createBody(createCircle(0.1), gd.x, gd.y, 1, true)
        worldAddBody(world, pivot)

        local joint = createRevoluteJoint(pivot, gear, vec(0, 0), vec(0, 0))
        if i == 1 then
            joint.motorEnabled = true
            joint.motorSpeed = 5
            joint.maxMotorTorque = 100
        end
        worldAddJoint(world, joint)
        gearJoints[i] = joint
    end

    for i = 1, #gearBodies - 1 do
        local ratio = -gearData[i].radius / gearData[i + 1].radius
        local gj = createGearJoint(gearJoints[i], gearJoints[i + 1], ratio)
        worldAddJoint(world, gj)
    end

    return world
end

-- ============================================================================
-- Scenario 12: Cloth simulation (grid of distance joints)
-- ============================================================================

function createClothScenario()
    local world = createWorld(vec(0, -5), 2.0)

    local cols = 10
    local rows = 8
    local spacing = 0.8
    local startX = -(cols - 1) * spacing / 2
    local startY = 12

    local particles = {}
    for r = 0, rows - 1 do
        particles[r] = {}
        for c = 0, cols - 1 do
            local x = startX + c * spacing
            local y = startY - r * spacing
            local isFixed = (r == 0) and (c == 0 or c == cols - 1 or c == math_floor(cols / 2))
            local p = createBody(createCircle(0.1), x, y, 0.5, isFixed)
            p.linearDamping = 0.3
            p.angularDamping = 0.5
            worldAddBody(world, p)
            particles[r][c] = p
        end
    end

    for r = 0, rows - 1 do
        for c = 0, cols - 1 do
            if c < cols - 1 then
                local joint = createDistanceJoint(
                    particles[r][c], particles[r][c + 1],
                    vec(0, 0), vec(0, 0), spacing)
                joint.stiffness = 150
                joint.damping = 3
                worldAddJoint(world, joint)
            end
            if r < rows - 1 then
                local joint = createDistanceJoint(
                    particles[r][c], particles[r + 1][c],
                    vec(0, 0), vec(0, 0), spacing)
                joint.stiffness = 150
                joint.damping = 3
                worldAddJoint(world, joint)
            end
        end
    end

    local obstacle = createBody(createCircle(2.0), 0, 7, 1, true)
    worldAddBody(world, obstacle)

    return world
end

-- ============================================================================
-- Scenario 13: Conveyor belt (applying tangential force at contacts)
-- ============================================================================

function createConveyorScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(25, 0.5), 0, -0.5, 1, true)
    worldAddBody(world, ground)

    local belt1 = createBody(createBox(6, 0.3), -5, 2, 1, true)
    belt1.angle = -0.15
    belt1.dynamicFriction = 0.9
    belt1.userData = {beltSpeed = 3.0}
    worldAddBody(world, belt1)

    local belt2 = createBody(createBox(6, 0.3), 7, 4, 1, true)
    belt2.angle = 0.1
    belt2.dynamicFriction = 0.9
    belt2.userData = {beltSpeed = -2.0}
    worldAddBody(world, belt2)

    local belt3 = createBody(createBox(5, 0.3), -2, 7, 1, true)
    belt3.angle = -0.05
    belt3.dynamicFriction = 0.9
    belt3.userData = {beltSpeed = 4.0}
    worldAddBody(world, belt3)

    resetRandom()
    for i = 1, 20 do
        local shapeChoice = math_floor(random() * 3)
        local x = randomRange(-8, -4)
        local y = randomRange(9, 14)
        local body
        if shapeChoice == 0 then
            body = createBody(createCircle(randomRange(0.2, 0.5)), x, y, 2.0, false)
        elseif shapeChoice == 1 then
            body = createBody(createBox(randomRange(0.2, 0.5), randomRange(0.2, 0.5)), x, y, 2.0, false)
        else
            body = createBody(createRegularPolygon(randomRange(0.3, 0.5), 5), x, y, 2.0, false)
        end
        body.dynamicFriction = 0.5
        body.restitution = 0.2
        worldAddBody(world, body)
    end

    return world
end

-- ============================================================================
-- Scenario 14: Catapult (prismatic joint + release mechanism)
-- ============================================================================

function createCatapultScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(30, 1), 0, -1, 1, true)
    worldAddBody(world, ground)

    local baseX = -10
    local baseY = 0

    local base = createBody(createBox(2, 0.5), baseX, baseY + 0.5, 1, true)
    worldAddBody(world, base)

    local arm = createBody(createBox(4, 0.2), baseX, baseY + 1.5, 3.0, false)
    worldAddBody(world, arm)

    local pivot = createRevoluteJoint(base, arm, vec(0, 0.5), vec(-2, 0))
    worldAddJoint(world, pivot)

    local counterweight = createBody(createBox(0.8, 0.8), baseX - 3, baseY + 2, 20.0, false)
    worldAddBody(world, counterweight)
    local cwJoint = createWeldJoint(arm, counterweight, vec(-2.5, 0), vec(0, 0))
    worldAddJoint(world, cwJoint)

    local projectile = createBody(createCircle(0.4), baseX + 3.5, baseY + 2, 1.0, false)
    projectile.restitution = 0.3
    worldAddBody(world, projectile)

    local cupJoint = createDistanceJoint(arm, projectile, vec(3.5, 0.2), vec(0, 0), 0.3)
    cupJoint.stiffness = 300
    cupJoint.damping = 5
    worldAddJoint(world, cupJoint)

    local targetX = 10
    for row = 0, 4 do
        for col = 0, 3 do
            local x = targetX + col * 0.8
            local y = 0.3 + row * 0.6
            local target = createBody(createBox(0.35, 0.25), x, y, 1.5, false)
            target.restitution = 0.1
            worldAddBody(world, target)
        end
    end

    arm.angularVelocity = -8

    return world
end

-- ============================================================================
-- Scenario 15: Pinball machine (flippers, bumpers, ball)
-- ============================================================================

function createPinballScenario()
    local world = createWorld(vec(0, -8), 2.5)

    local tableAngle = 0.1
    local tableW = 10
    local tableH = 20

    local leftWall = createBody(createBox(0.3, tableH / 2), -tableW / 2 - 0.3, tableH / 2, 1, true)
    worldAddBody(world, leftWall)
    local rightWall = createBody(createBox(0.3, tableH / 2), tableW / 2 + 0.3, tableH / 2, 1, true)
    worldAddBody(world, rightWall)
    local topWall = createBody(createBox(tableW / 2, 0.3), 0, tableH + 0.3, 1, true)
    worldAddBody(world, topWall)

    local drainVerts = {
        vec(-tableW / 2, 0), vec(-2, -1.5), vec(2, -1.5), vec(tableW / 2, 0)
    }
    for i = 1, 3 do
        local mid = vecLerp(drainVerts[i], drainVerts[i + 1], 0.5)
        local dx = drainVerts[i + 1].x - drainVerts[i].x
        local dy = drainVerts[i + 1].y - drainVerts[i].y
        local len = math_sqrt(dx * dx + dy * dy)
        local wall = createBody(createBox(len / 2, 0.2), mid.x, mid.y, 1, true)
        wall.angle = math_atan2(dy, dx)
        worldAddBody(world, wall)
    end

    local bumperPositions = {
        {x = 0, y = 14}, {x = -2.5, y = 12}, {x = 2.5, y = 12},
        {x = -1.5, y = 9}, {x = 1.5, y = 9}, {x = 0, y = 7},
        {x = -3, y = 6}, {x = 3, y = 6}
    }

    for i = 1, #bumperPositions do
        local bp = bumperPositions[i]
        local bumper = createBody(createCircle(0.6), bp.x, bp.y, 1, true)
        bumper.restitution = 1.2
        worldAddBody(world, bumper)
    end

    local leftFlipper = createBody(createBox(1.5, 0.2), -2, 2, 5.0, false)
    leftFlipper.angularDamping = 2.0
    worldAddBody(world, leftFlipper)
    local lfPivot = createRevoluteJoint(leftWall, leftFlipper, vec(0.3, 2), vec(-1.2, 0))
    lfPivot.motorEnabled = true
    lfPivot.motorSpeed = 20
    lfPivot.maxMotorTorque = 200
    worldAddJoint(world, lfPivot)

    local rightFlipper = createBody(createBox(1.5, 0.2), 2, 2, 5.0, false)
    rightFlipper.angularDamping = 2.0
    worldAddBody(world, rightFlipper)
    local rfPivot = createRevoluteJoint(rightWall, rightFlipper, vec(-0.3, 2), vec(1.2, 0))
    rfPivot.motorEnabled = true
    rfPivot.motorSpeed = -20
    rfPivot.maxMotorTorque = 200
    worldAddJoint(world, rfPivot)

    local ball = createBody(createCircle(0.35), 4, 18, 2.0, false)
    ball.restitution = 0.7
    ball.linearDamping = 0.05
    ball.velocity = vec(-3, -2)
    worldAddBody(world, ball)

    local ball2 = createBody(createCircle(0.35), -3, 16, 2.0, false)
    ball2.restitution = 0.7
    ball2.linearDamping = 0.05
    ball2.velocity = vec(2, -4)
    worldAddBody(world, ball2)

    return world
end

-- ============================================================================
-- Scenario 16: Rube Goldberg machine
-- ============================================================================

function createRubeGoldbergScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(40, 0.5), 0, -0.5, 1, true)
    worldAddBody(world, ground)

    local ramp1 = createBody(createBox(4, 0.2), -12, 8, 1, true)
    ramp1.angle = -0.3
    worldAddBody(world, ramp1)

    local ball1 = createBody(createCircle(0.4), -15, 10, 3.0, false)
    ball1.restitution = 0.5
    worldAddBody(world, ball1)

    local seesaw = createBody(createBox(3, 0.15), -6, 4, 2.0, false)
    worldAddBody(world, seesaw)
    local seesawPivot = createBody(createCircle(0.1), -6, 4, 1, true)
    worldAddBody(world, seesawPivot)
    local seesawJoint = createRevoluteJoint(seesawPivot, seesaw, vec(0, 0), vec(0, 0))
    worldAddJoint(world, seesawJoint)

    local weight = createBody(createBox(0.5, 0.5), -8.5, 5, 8.0, false)
    worldAddBody(world, weight)

    local ramp2 = createBody(createBox(3, 0.2), -2, 6, 1, true)
    ramp2.angle = 0.25
    worldAddBody(world, ramp2)

    local ramp3 = createBody(createBox(3, 0.2), 3, 4, 1, true)
    ramp3.angle = -0.2
    worldAddBody(world, ramp3)

    local numDominoes = 8
    for i = 0, numDominoes - 1 do
        local x = 7 + i * 0.9
        local domino = createBody(createBox(0.1, 0.7), x, 0.7, 3.0, false)
        domino.staticFriction = 0.5
        worldAddBody(world, domino)
    end

    local pendulumAnchor = createBody(createCircle(0.1), 5, 10, 1, true)
    worldAddBody(world, pendulumAnchor)
    local pendulumBall = createBody(createCircle(0.5), 5, 6, 5.0, false)
    worldAddBody(world, pendulumBall)
    local pendulumJoint = createDistanceJoint(pendulumAnchor, pendulumBall, vec(0, 0), vec(0, 0), 4)
    pendulumJoint.stiffness = 500
    pendulumJoint.damping = 1
    worldAddJoint(world, pendulumJoint)

    local bucket = createBody(createBox(1, 0.1), 15, 3, 2.0, false)
    worldAddBody(world, bucket)
    local bucketLeft = createBody(createBox(0.1, 0.5), 14, 3.5, 2.0, false)
    worldAddBody(world, bucketLeft)
    local bucketRight = createBody(createBox(0.1, 0.5), 16, 3.5, 2.0, false)
    worldAddBody(world, bucketRight)
    local bwl = createWeldJoint(bucket, bucketLeft, vec(-1, 0), vec(0, -0.4))
    worldAddJoint(world, bwl)
    local bwr = createWeldJoint(bucket, bucketRight, vec(1, 0), vec(0, -0.4))
    worldAddJoint(world, bwr)

    local bucketRope = createRopeJoint(ground, bucket, vec(15, 8), vec(0, 0), 5)
    worldAddJoint(world, bucketRope)

    return world
end

-- ============================================================================
-- Scenario 17: Granular material (many small circles)
-- ============================================================================

function createGranularScenario()
    local world = createWorld(vec(0, -10), 1.5)

    local funnel_left = createBody(createBox(3, 0.2), -3, 12, 1, true)
    funnel_left.angle = 0.6
    worldAddBody(world, funnel_left)
    local funnel_right = createBody(createBox(3, 0.2), 3, 12, 1, true)
    funnel_right.angle = -0.6
    worldAddBody(world, funnel_right)

    local channel_left = createBody(createBox(0.2, 4), -0.8, 8, 1, true)
    worldAddBody(world, channel_left)
    local channel_right = createBody(createBox(0.2, 4), 0.8, 8, 1, true)
    worldAddBody(world, channel_right)

    local container_left = createBody(createBox(0.2, 3), -4, 1.5, 1, true)
    worldAddBody(world, container_left)
    local container_right = createBody(createBox(0.2, 3), 4, 1.5, 1, true)
    worldAddBody(world, container_right)
    local container_bottom = createBody(createBox(4, 0.2), 0, -0.7, 1, true)
    worldAddBody(world, container_bottom)

    local deflector = createBody(createRegularPolygon(0.8, 3), 0, 5, 1, true)
    worldAddBody(world, deflector)

    resetRandom()
    for i = 1, 60 do
        local radius = randomRange(0.15, 0.3)
        local x = randomRange(-1.5, 1.5)
        local y = randomRange(13, 20)
        local grain = createBody(createCircle(radius), x, y, 2.5, false)
        grain.restitution = 0.1
        grain.dynamicFriction = 0.4
        grain.linearDamping = 0.02
        worldAddBody(world, grain)
    end

    return world
end

-- ============================================================================
-- Scenario 18: Ragdoll (connected body segments)
-- ============================================================================

function createRagdollScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(20, 0.5), 0, -0.5, 1, true)
    worldAddBody(world, ground)

    local platform = createBody(createBox(3, 0.2), 0, 8, 1, true)
    worldAddBody(world, platform)

    local function makeRagdoll(startX, startY, scale)
        local headRadius = 0.3 * scale
        local torsoW = 0.35 * scale
        local torsoH = 0.6 * scale
        local limbW = 0.15 * scale
        local upperLimbH = 0.4 * scale
        local lowerLimbH = 0.35 * scale

        local head = createBody(createCircle(headRadius), startX, startY, 2.0, false)
        head.angularDamping = 0.3
        worldAddBody(world, head)

        local torso = createBody(createBox(torsoW, torsoH), startX, startY - headRadius - torsoH, 3.0, false)
        worldAddBody(world, torso)
        local neckJoint = createRevoluteJoint(head, torso,
            vec(0, -headRadius), vec(0, torsoH))
        worldAddJoint(world, neckJoint)

        local upperArmL = createBody(createBox(limbW, upperLimbH),
            startX - torsoW - limbW, startY - headRadius - 0.1, 1.5, false)
        worldAddBody(world, upperArmL)
        local shoulderL = createRevoluteJoint(torso, upperArmL,
            vec(-torsoW, torsoH - 0.1), vec(0, upperLimbH))
        worldAddJoint(world, shoulderL)

        local lowerArmL = createBody(createBox(limbW, lowerLimbH),
            startX - torsoW - limbW, startY - headRadius - 0.1 - upperLimbH * 2, 1.0, false)
        worldAddBody(world, lowerArmL)
        local elbowL = createRevoluteJoint(upperArmL, lowerArmL,
            vec(0, -upperLimbH), vec(0, lowerLimbH))
        worldAddJoint(world, elbowL)

        local upperArmR = createBody(createBox(limbW, upperLimbH),
            startX + torsoW + limbW, startY - headRadius - 0.1, 1.5, false)
        worldAddBody(world, upperArmR)
        local shoulderR = createRevoluteJoint(torso, upperArmR,
            vec(torsoW, torsoH - 0.1), vec(0, upperLimbH))
        worldAddJoint(world, shoulderR)

        local lowerArmR = createBody(createBox(limbW, lowerLimbH),
            startX + torsoW + limbW, startY - headRadius - 0.1 - upperLimbH * 2, 1.0, false)
        worldAddBody(world, lowerArmR)
        local elbowR = createRevoluteJoint(upperArmR, lowerArmR,
            vec(0, -upperLimbH), vec(0, lowerLimbH))
        worldAddJoint(world, elbowR)

        local upperLegL = createBody(createBox(limbW, upperLimbH),
            startX - torsoW * 0.5, startY - headRadius - torsoH * 2 - 0.1, 2.0, false)
        worldAddBody(world, upperLegL)
        local hipL = createRevoluteJoint(torso, upperLegL,
            vec(-torsoW * 0.5, -torsoH), vec(0, upperLimbH))
        worldAddJoint(world, hipL)

        local lowerLegL = createBody(createBox(limbW, lowerLimbH),
            startX - torsoW * 0.5, startY - headRadius - torsoH * 2 - upperLimbH * 2 - 0.1, 1.5, false)
        worldAddBody(world, lowerLegL)
        local kneeL = createRevoluteJoint(upperLegL, lowerLegL,
            vec(0, -upperLimbH), vec(0, lowerLimbH))
        worldAddJoint(world, kneeL)

        local upperLegR = createBody(createBox(limbW, upperLimbH),
            startX + torsoW * 0.5, startY - headRadius - torsoH * 2 - 0.1, 2.0, false)
        worldAddBody(world, upperLegR)
        local hipR = createRevoluteJoint(torso, upperLegR,
            vec(torsoW * 0.5, -torsoH), vec(0, upperLimbH))
        worldAddJoint(world, hipR)

        local lowerLegR = createBody(createBox(limbW, lowerLimbH),
            startX + torsoW * 0.5, startY - headRadius - torsoH * 2 - upperLimbH * 2 - 0.1, 1.5, false)
        worldAddBody(world, lowerLegR)
        local kneeR = createRevoluteJoint(upperLegR, lowerLegR,
            vec(0, -upperLimbH), vec(0, lowerLimbH))
        worldAddJoint(world, kneeR)
    end

    makeRagdoll(-3, 12, 1.0)
    makeRagdoll(0, 14, 1.2)
    makeRagdoll(3, 11, 0.9)

    return world
end

-- ============================================================================
-- Scenario 19: Breakable joint chain (stress test)
-- ============================================================================

function createBreakableChainScenario()
    local world = createWorld(vec(0, -10), 2.5)

    local ground = createBody(createBox(20, 0.5), 0, -0.5, 1, true)
    worldAddBody(world, ground)

    local numChains = 5
    local linksPerChain = 10
    local chainSpacing = 4
    local startX = -(numChains - 1) * chainSpacing / 2

    for chain = 0, numChains - 1 do
        local x = startX + chain * chainSpacing
        local anchor = createBody(createCircle(0.2), x, 15, 1, true)
        worldAddBody(world, anchor)

        local prev = anchor
        for link = 1, linksPerChain do
            local linkBody = createBody(createBox(0.3, 0.15), x, 15 - link * 0.7, 2.0, false)
            linkBody.angularDamping = 0.1
            worldAddBody(world, linkBody)

            local joint = createDistanceJoint(prev, linkBody,
                vec(0, link == 1 and 0 or -0.15), vec(0, 0.15), 0.4)
            joint.stiffness = 200
            joint.damping = 5
            worldAddJoint(world, joint)
            prev = linkBody
        end

        local weight = createBody(createCircle(0.6), x, 15 - (linksPerChain + 1) * 0.7, 10.0, false)
        worldAddBody(world, weight)
        local endJoint = createDistanceJoint(prev, weight, vec(0, -0.15), vec(0, 0.3), 0.3)
        endJoint.stiffness = 200
        endJoint.damping = 5
        worldAddJoint(world, endJoint)
    end

    local striker = createBody(createCircle(1.0), -15, 8, 20.0, false)
    striker.velocity = vec(20, 0)
    striker.restitution = 0.3
    worldAddBody(world, striker)

    return world
end

-- ============================================================================
-- Scenario 20: Stacking with varying shapes (stress test for solver)
-- ============================================================================

function createMixedStackScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(20, 0.5), 0, -0.5, 1, true)
    ground.staticFriction = 0.9
    worldAddBody(world, ground)

    resetRandom()
    local y = 0.5
    for layer = 1, 15 do
        local numItems = math_max(1, 6 - math_floor(layer / 3))
        local totalWidth = numItems * 1.8
        local startX = -totalWidth / 2

        for item = 0, numItems - 1 do
            local x = startX + item * 1.8 + 0.9
            local shapeChoice = math_floor(random() * 4)
            local body

            if shapeChoice == 0 then
                body = createBody(createCircle(randomRange(0.3, 0.6)), x, y + 0.5, 2.0, false)
            elseif shapeChoice == 1 then
                body = createBody(createBox(randomRange(0.4, 0.8), randomRange(0.3, 0.5)), x, y + 0.4, 2.0, false)
            elseif shapeChoice == 2 then
                body = createBody(createRegularPolygon(randomRange(0.3, 0.6), 5), x, y + 0.5, 2.0, false)
            else
                body = createBody(createRegularPolygon(randomRange(0.3, 0.6), 3), x, y + 0.5, 2.0, false)
            end

            body.restitution = 0.0
            body.staticFriction = 0.7
            body.dynamicFriction = 0.5
            worldAddBody(world, body)
        end
        y = y + 1.1
    end

    return world
end

-- ============================================================================
-- Additional raycast and query test scenario
-- ============================================================================

function createRaycastTestScenario()
    local world = createWorld(vec(0, 0), 3.0)
    world.gravity = vec(0, 0)

    resetRandom()
    for i = 1, 30 do
        local x = randomRange(-15, 15)
        local y = randomRange(-10, 10)
        local shapeChoice = math_floor(random() * 3)
        local body
        if shapeChoice == 0 then
            body = createBody(createCircle(randomRange(0.5, 1.5)), x, y, 1.0, true)
        elseif shapeChoice == 1 then
            body = createBody(createBox(randomRange(0.5, 2.0), randomRange(0.5, 2.0)), x, y, 1.0, true)
        else
            body = createBody(createRegularPolygon(randomRange(0.5, 1.5), math_floor(random() * 4) + 3), x, y, 1.0, true)
        end
        body.angle = randomRange(0, math_pi * 2)
        worldAddBody(world, body)
    end

    local rayResults = {}
    local numRays = 50
    for i = 1, numRays do
        local angle = (i - 1) * math_pi * 2 / numRays
        local dir = vec(math_cos(angle), math_sin(angle))
        local hit = worldRaycast(world, vec(0, 0), dir, 20)
        if hit then
            rayResults[#rayResults + 1] = hit.t
        end
    end

    local aabbResults = worldQueryAABB(world, {minX = -5, minY = -5, maxX = 5, maxY = 5})
    local pointResults = worldQueryPoint(world, vec(0, 0))

    return world, #rayResults, #aabbResults, #pointResults
end

-- ============================================================================
-- Particle System (Verlet integration, no rotation)
-- ============================================================================

local function createParticle(x, y, mass, radius)
    return {
        pos = vec(x, y),
        prevPos = vec(x, y),
        acc = vec(0, 0),
        mass = mass,
        invMass = mass > 0 and 1 / mass or 0,
        radius = radius,
        pinned = false
    }
end

local function createParticleConstraint(p1, p2, restLength, stiffness)
    return {
        p1 = p1,
        p2 = p2,
        restLength = restLength,
        stiffness = stiffness or 1.0
    }
end

local function particleSystemStep(particles, constraints, gravity, dt, bounds)
    for i = 1, #particles do
        local p = particles[i]
        if not p.pinned then
            p.acc = vecAdd(p.acc, gravity)
            local vel = vecSub(p.pos, p.prevPos)
            vel = vecMul(vel, 0.99)
            p.prevPos = {x = p.pos.x, y = p.pos.y}
            p.pos = vecAdd(vecAdd(p.pos, vel), vecMul(p.acc, dt * dt))
            p.acc = vec(0, 0)
        end
    end

    local iterations = 4
    for iter = 1, iterations do
        for i = 1, #constraints do
            local c = constraints[i]
            local diff = vecSub(c.p2.pos, c.p1.pos)
            local dist = vecLen(diff)
            if dist > 0.001 then
                local error = (dist - c.restLength) / dist
                local correction = vecMul(diff, error * 0.5 * c.stiffness)
                if not c.p1.pinned then
                    c.p1.pos = vecAdd(c.p1.pos, correction)
                end
                if not c.p2.pinned then
                    c.p2.pos = vecSub(c.p2.pos, correction)
                end
            end
        end

        for i = 1, #particles do
            local p = particles[i]
            if not p.pinned and bounds then
                if p.pos.x - p.radius < bounds.minX then p.pos.x = bounds.minX + p.radius end
                if p.pos.x + p.radius > bounds.maxX then p.pos.x = bounds.maxX - p.radius end
                if p.pos.y - p.radius < bounds.minY then p.pos.y = bounds.minY + p.radius end
                if p.pos.y + p.radius > bounds.maxY then p.pos.y = bounds.maxY - p.radius end
            end
        end

        for i = 1, #particles do
            for j = i + 1, #particles do
                local p1 = particles[i]
                local p2 = particles[j]
                local diff = vecSub(p2.pos, p1.pos)
                local dist = vecLen(diff)
                local minDist = p1.radius + p2.radius
                if dist < minDist and dist > 0.001 then
                    local overlap = (minDist - dist) / dist
                    local correction = vecMul(diff, overlap * 0.5)
                    if not p1.pinned then
                        p1.pos = vecSub(p1.pos, correction)
                    end
                    if not p2.pinned then
                        p2.pos = vecAdd(p2.pos, correction)
                    end
                end
            end
        end
    end
end

local function checksumParticles(particles)
    local sum = 0
    for i = 1, #particles do
        sum = sum + particles[i].pos.x * 100 + particles[i].pos.y * 100
    end
    return math_floor(sum * 100) / 100
end

-- ============================================================================
-- Scenario 21: Particle rope (Verlet)
-- ============================================================================

function createParticleRopeScenario()
    local numParticles = 40
    local spacing = 0.5
    local particles = {}
    local constraints = {}

    for i = 1, numParticles do
        local p = createParticle((i - 1) * spacing, 10, 1.0, 0.1)
        if i == 1 then p.pinned = true end
        particles[i] = p
    end

    for i = 1, numParticles - 1 do
        constraints[i] = createParticleConstraint(particles[i], particles[i + 1], spacing, 1.0)
    end

    local gravity = vec(0, -10)
    local bounds = {minX = -5, minY = -5, maxX = 25, maxY = 15}

    for step = 1, 60 do
        particleSystemStep(particles, constraints, gravity, 1/60, bounds)
    end

    return checksumParticles(particles)
end

-- ============================================================================
-- Scenario 22: Particle cloth (2D grid with Verlet)
-- ============================================================================

function createParticleClothScenario()
    local cols = 15
    local rows = 12
    local spacing = 0.4
    local particles = {}
    local constraints = {}

    for r = 0, rows - 1 do
        for c = 0, cols - 1 do
            local idx = r * cols + c + 1
            local p = createParticle(c * spacing, 8 - r * spacing, 1.0, 0.05)
            if r == 0 and (c == 0 or c == cols - 1 or c == math_floor(cols / 2)) then
                p.pinned = true
            end
            particles[idx] = p
        end
    end

    for r = 0, rows - 1 do
        for c = 0, cols - 1 do
            local idx = r * cols + c + 1
            if c < cols - 1 then
                constraints[#constraints + 1] = createParticleConstraint(
                    particles[idx], particles[idx + 1], spacing, 0.9)
            end
            if r < rows - 1 then
                constraints[#constraints + 1] = createParticleConstraint(
                    particles[idx], particles[idx + cols], spacing, 0.9)
            end
            if c < cols - 1 and r < rows - 1 then
                local diagLen = spacing * 1.414
                constraints[#constraints + 1] = createParticleConstraint(
                    particles[idx], particles[idx + cols + 1], diagLen, 0.5)
            end
            if c > 0 and r < rows - 1 then
                local diagLen = spacing * 1.414
                constraints[#constraints + 1] = createParticleConstraint(
                    particles[idx], particles[idx + cols - 1], diagLen, 0.5)
            end
        end
    end

    local gravity = vec(0, -5)
    local bounds = {minX = -3, minY = -3, maxX = 10, maxY = 10}

    for step = 1, 50 do
        particleSystemStep(particles, constraints, gravity, 1/60, bounds)
    end

    return checksumParticles(particles)
end

-- ============================================================================
-- Scenario 23: Soft body (particle-based circle)
-- ============================================================================

function createSoftBodyScenario()
    local numRings = 3
    local particlesPerRing = {12, 8, 4}
    local ringRadii = {2.0, 1.3, 0.6}
    local centerX, centerY = 0, 8

    local allParticles = {}
    local allConstraints = {}

    local center = createParticle(centerX, centerY, 2.0, 0.15)
    allParticles[1] = center

    for ring = 1, numRings do
        local n = particlesPerRing[ring]
        local r = ringRadii[ring]
        local startIdx = #allParticles + 1
        for i = 1, n do
            local angle = (i - 1) * 2 * math_pi / n
            local px = centerX + r * math_cos(angle)
            local py = centerY + r * math_sin(angle)
            local p = createParticle(px, py, 1.0, 0.12)
            allParticles[#allParticles + 1] = p
        end

        for i = 0, n - 1 do
            local idx1 = startIdx + i
            local idx2 = startIdx + (i + 1) % n
            local dist = vecDist(allParticles[idx1].pos, allParticles[idx2].pos)
            allConstraints[#allConstraints + 1] = createParticleConstraint(
                allParticles[idx1], allParticles[idx2], dist, 0.8)
        end

        for i = 0, n - 1 do
            local idx = startIdx + i
            local dist = vecDist(allParticles[idx].pos, center.pos)
            allConstraints[#allConstraints + 1] = createParticleConstraint(
                allParticles[idx], center, dist, 0.6)
        end
    end

    for i = 1, particlesPerRing[1] do
        local outerIdx = 1 + i
        local innerIdx = 1 + particlesPerRing[1] + math_floor((i - 1) * particlesPerRing[2] / particlesPerRing[1]) + 1
        if innerIdx <= 1 + particlesPerRing[1] + particlesPerRing[2] then
            local dist = vecDist(allParticles[outerIdx].pos, allParticles[innerIdx].pos)
            allConstraints[#allConstraints + 1] = createParticleConstraint(
                allParticles[outerIdx], allParticles[innerIdx], dist, 0.5)
        end
    end

    local gravity = vec(0, -10)
    local bounds = {minX = -5, minY = -2, maxX = 5, maxY = 12}

    for step = 1, 60 do
        particleSystemStep(allParticles, allConstraints, gravity, 1/60, bounds)
    end

    return checksumParticles(allParticles)
end

-- ============================================================================
-- Buoyancy simulation
-- ============================================================================

local function computeSubmergedArea(body, waterLevel)
    if body.shape.type == SHAPE_CIRCLE then
        local r = body.shape.radius
        local depth = waterLevel - (body.position.y - r)
        if depth <= 0 then return 0, vec(0, 0) end
        if depth >= 2 * r then return math_pi * r * r, body.position end
        local ratio = depth / (2 * r)
        local area = math_pi * r * r * ratio
        local centroidY = body.position.y - r + depth / 2
        return area, vec(body.position.x, centroidY)
    else
        local verts = bodyGetTransformedVertices(body)
        local n = #verts
        local submergedVerts = {}
        for i = 1, n do
            if verts[i].y <= waterLevel then
                submergedVerts[#submergedVerts + 1] = verts[i]
            end
        end
        for i = 1, n do
            local j = (i % n) + 1
            local v1 = verts[i]
            local v2 = verts[j]
            if (v1.y <= waterLevel) ~= (v2.y <= waterLevel) then
                local t = (waterLevel - v1.y) / (v2.y - v1.y)
                submergedVerts[#submergedVerts + 1] = vecLerp(v1, v2, t)
            end
        end
        if #submergedVerts < 3 then return 0, vec(0, 0) end

        local cx, cy = 0, 0
        for i = 1, #submergedVerts do
            cx = cx + submergedVerts[i].x
            cy = cy + submergedVerts[i].y
        end
        cx = cx / #submergedVerts
        cy = cy / #submergedVerts

        table.sort(submergedVerts, function(a, b)
            local angA = math_atan2(a.y - cy, a.x - cx)
            local angB = math_atan2(b.y - cy, b.x - cx)
            return angA < angB
        end)

        local area = computePolygonArea(submergedVerts)
        local centroid = computePolygonCentroid(submergedVerts)
        return area, centroid
    end
end

local function applyBuoyancy(body, waterLevel, waterDensity, dragCoeff)
    if body.isStatic then return end
    local subArea, buoyancyCenter = computeSubmergedArea(body, waterLevel)
    if subArea <= 0 then return end

    local buoyancyForce = vec(0, waterDensity * subArea * 10)
    bodyApplyForceAtPoint(body, buoyancyForce, buoyancyCenter)

    local vel = bodyGetVelocityAtPoint(body, buoyancyCenter)
    local dragForce = vecMul(vel, -dragCoeff * subArea)
    bodyApplyForceAtPoint(body, dragForce, buoyancyCenter)

    body.angularVelocity = body.angularVelocity * (1 - 0.02 * subArea)
end

-- ============================================================================
-- Scenario 24: Buoyancy pool
-- ============================================================================

function createBuoyancyScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local poolLeft = createBody(createBox(0.5, 5), -8, 2.5, 1, true)
    worldAddBody(world, poolLeft)
    local poolRight = createBody(createBox(0.5, 5), 8, 2.5, 1, true)
    worldAddBody(world, poolRight)
    local poolBottom = createBody(createBox(8, 0.5), 0, -2, 1, true)
    worldAddBody(world, poolBottom)

    resetRandom()
    local floaters = {}
    for i = 1, 15 do
        local shapeChoice = math_floor(random() * 3)
        local x = randomRange(-6, 6)
        local y = randomRange(3, 8)
        local body
        if shapeChoice == 0 then
            body = createBody(createCircle(randomRange(0.3, 0.8)), x, y, randomRange(0.3, 1.5), false)
        elseif shapeChoice == 1 then
            body = createBody(createBox(randomRange(0.4, 1.0), randomRange(0.3, 0.6)), x, y, randomRange(0.3, 1.5), false)
        else
            body = createBody(createRegularPolygon(randomRange(0.4, 0.7), 5), x, y, randomRange(0.3, 1.5), false)
        end
        body.restitution = 0.2
        worldAddBody(world, body)
        floaters[#floaters + 1] = body
    end

    world.waterLevel = 5.0
    world.waterDensity = 1.0
    world.dragCoeff = 2.0
    world.floaters = floaters

    return world
end

-- ============================================================================
-- Scenario 25: Tornado / vortex (radial force field)
-- ============================================================================

function createTornadoScenario()
    local world = createWorld(vec(0, -5), 3.0)

    local ground = createBody(createBox(20, 0.5), 0, -0.5, 1, true)
    worldAddBody(world, ground)

    local wallL = createBody(createBox(0.5, 10), -10, 5, 1, true)
    worldAddBody(world, wallL)
    local wallR = createBody(createBox(0.5, 10), 10, 5, 1, true)
    worldAddBody(world, wallR)
    local ceiling = createBody(createBox(20, 0.5), 0, 15, 1, true)
    worldAddBody(world, ceiling)

    local debris = {}
    resetRandom()
    for i = 1, 40 do
        local x = randomRange(-8, 8)
        local y = randomRange(0.5, 3)
        local body
        local sc = math_floor(random() * 3)
        if sc == 0 then
            body = createBody(createCircle(randomRange(0.2, 0.5)), x, y, 1.5, false)
        elseif sc == 1 then
            body = createBody(createBox(randomRange(0.2, 0.6), randomRange(0.2, 0.6)), x, y, 1.5, false)
        else
            body = createBody(createRegularPolygon(randomRange(0.2, 0.5), math_floor(random() * 3) + 3), x, y, 1.5, false)
        end
        body.linearDamping = 0.1
        body.angularDamping = 0.1
        worldAddBody(world, body)
        debris[#debris + 1] = body
    end

    world.vortexCenter = vec(0, 7)
    world.vortexStrength = 30
    world.debris = debris

    return world
end

-- ============================================================================
-- Scenario 26: Pyramid stress test (many resting contacts)
-- ============================================================================

function createLargePyramidScenario()
    local world = createWorld(vec(0, -10), 2.0)
    world.iterations = 15

    local ground = createBody(createBox(30, 0.5), 0, -0.5, 1, true)
    ground.staticFriction = 0.9
    worldAddBody(world, ground)

    local baseWidth = 20
    local boxSize = 0.45
    local spacing = boxSize * 2.05
    local row = 0
    local y = 0.5

    while true do
        local numBoxes = baseWidth - row
        if numBoxes <= 0 then break end
        local startX = -(numBoxes - 1) * spacing / 2
        for col = 0, numBoxes - 1 do
            local x = startX + col * spacing
            local box = createBody(createBox(boxSize, boxSize), x, y, 2.0, false)
            box.restitution = 0.0
            box.staticFriction = 0.7
            box.dynamicFriction = 0.5
            worldAddBody(world, box)
        end
        y = y + spacing
        row = row + 1
    end

    return world
end

-- ============================================================================
-- Scenario 27: Marble run (ramps + funnels + obstacles)
-- ============================================================================

function createMarbleRunScenario()
    local world = createWorld(vec(0, -10), 2.5)

    local ramps = {
        {x = -5, y = 18, w = 6, angle = -0.2},
        {x = 5, y = 15, w = 6, angle = 0.25},
        {x = -4, y = 12, w = 5, angle = -0.15},
        {x = 4, y = 9, w = 5, angle = 0.2},
        {x = -3, y = 6, w = 5, angle = -0.25},
        {x = 3, y = 3, w = 4, angle = 0.15},
    }

    for i = 1, #ramps do
        local r = ramps[i]
        local ramp = createBody(createBox(r.w / 2, 0.15), r.x, r.y, 1, true)
        ramp.angle = r.angle
        ramp.restitution = 0.3
        worldAddBody(world, ramp)

        local lip = createBody(createBox(0.15, 0.3), r.x + r.w / 2 * math_cos(r.angle), r.y + r.w / 2 * math_sin(r.angle), 1, true)
        worldAddBody(world, lip)
    end

    local obstacles = {
        {x = 0, y = 16.5, type = "circle", r = 0.4},
        {x = -2, y = 13.5, type = "triangle", r = 0.5},
        {x = 2, y = 10.5, type = "circle", r = 0.3},
        {x = -1, y = 7.5, type = "pentagon", r = 0.4},
        {x = 1, y = 4.5, type = "circle", r = 0.35},
    }

    for i = 1, #obstacles do
        local o = obstacles[i]
        local body
        if o.type == "circle" then
            body = createBody(createCircle(o.r), o.x, o.y, 1, true)
        elseif o.type == "triangle" then
            body = createBody(createRegularPolygon(o.r, 3), o.x, o.y, 1, true)
        else
            body = createBody(createRegularPolygon(o.r, 5), o.x, o.y, 1, true)
        end
        body.restitution = 0.6
        worldAddBody(world, body)
    end

    local floor = createBody(createBox(10, 0.3), 0, -0.3, 1, true)
    worldAddBody(world, floor)

    local collector_l = createBody(createBox(0.2, 1), -3, 0.7, 1, true)
    worldAddBody(world, collector_l)
    local collector_r = createBody(createBox(0.2, 1), 3, 0.7, 1, true)
    worldAddBody(world, collector_r)

    resetRandom()
    for i = 1, 25 do
        local radius = randomRange(0.2, 0.4)
        local x = randomRange(-7, -3)
        local y = randomRange(19, 22)
        local marble = createBody(createCircle(radius), x, y, 2.5, false)
        marble.restitution = randomRange(0.3, 0.7)
        marble.dynamicFriction = 0.2
        worldAddBody(world, marble)
    end

    return world
end

-- ============================================================================
-- Scenario 28: Explosion (radial impulse)
-- ============================================================================

function createExplosionScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(25, 0.5), 0, -0.5, 1, true)
    worldAddBody(world, ground)

    local wallSpacing = 3
    for wall = 1, 4 do
        local wallX = wall * wallSpacing - 7.5
        for row = 0, 5 do
            for col = 0, 2 do
                local x = wallX + col * 0.7
                local y = 0.3 + row * 0.6
                local brick = createBody(createBox(0.3, 0.25), x, y, 2.0, false)
                brick.restitution = 0.1
                brick.staticFriction = 0.6
                worldAddBody(world, brick)
            end
        end
    end

    local explosionCenter = vec(0, 1)
    local explosionRadius = 8
    local explosionForce = 500

    for i = 1, #world.bodies do
        local body = world.bodies[i]
        if not body.isStatic then
            local toBody = vecSub(body.position, explosionCenter)
            local dist = vecLen(toBody)
            if dist < explosionRadius and dist > 0.1 then
                local falloff = 1 - dist / explosionRadius
                local force = vecMul(vecNormalize(toBody), explosionForce * falloff * falloff)
                bodyApplyForce(body, force)
            end
        end
    end

    return world
end

-- ============================================================================
-- Scenario 29: Pulley system
-- ============================================================================

function createPulleyScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(20, 0.5), 0, -0.5, 1, true)
    worldAddBody(world, ground)

    local pulleyAnchor1 = createBody(createCircle(0.3), -5, 12, 1, true)
    worldAddBody(world, pulleyAnchor1)
    local pulleyAnchor2 = createBody(createCircle(0.3), 5, 12, 1, true)
    worldAddBody(world, pulleyAnchor2)

    local weight1 = createBody(createBox(1, 1), -5, 6, 5.0, false)
    worldAddBody(world, weight1)
    local rope1 = createRopeJoint(pulleyAnchor1, weight1, vec(0, 0), vec(0, 0.5), 6)
    worldAddJoint(world, rope1)

    local weight2 = createBody(createBox(0.8, 0.8), 5, 8, 3.0, false)
    worldAddBody(world, weight2)
    local rope2 = createRopeJoint(pulleyAnchor2, weight2, vec(0, 0), vec(0, 0.4), 4)
    worldAddJoint(world, rope2)

    local crossbar = createBody(createBox(5.5, 0.15), 0, 12.3, 1.0, false)
    crossbar.gravityScale = 0
    worldAddBody(world, crossbar)
    local cj1 = createDistanceJoint(pulleyAnchor1, crossbar, vec(0, 0.3), vec(-5, 0), 0.1)
    cj1.stiffness = 300
    cj1.damping = 10
    worldAddJoint(world, cj1)
    local cj2 = createDistanceJoint(pulleyAnchor2, crossbar, vec(0, 0.3), vec(5, 0), 0.1)
    cj2.stiffness = 300
    cj2.damping = 10
    worldAddJoint(world, cj2)

    local platform = createBody(createBox(3, 0.2), -5, 4.5, 2.0, false)
    worldAddBody(world, platform)
    local pj = createDistanceJoint(weight1, platform, vec(0, -0.5), vec(0, 0.2), 1.0)
    pj.stiffness = 200
    pj.damping = 5
    worldAddJoint(world, pj)

    for i = 1, 5 do
        local box = createBody(createBox(0.3, 0.3), -5 + (i - 3) * 0.65, 5.5, 1.5, false)
        worldAddBody(world, box)
    end

    return world
end

-- ============================================================================
-- Scenario 30: Elastic collision chain (demonstrates energy conservation)
-- ============================================================================

function createElasticChainScenario()
    local world = createWorld(vec(0, 0), 3.0)
    world.gravity = vec(0, 0)

    local wallTop = createBody(createBox(15, 0.3), 0, 5, 1, true)
    wallTop.restitution = 1.0
    worldAddBody(world, wallTop)
    local wallBot = createBody(createBox(15, 0.3), 0, -5, 1, true)
    wallBot.restitution = 1.0
    worldAddBody(world, wallBot)
    local wallL = createBody(createBox(0.3, 5), -15, 0, 1, true)
    wallL.restitution = 1.0
    worldAddBody(world, wallL)
    local wallR = createBody(createBox(0.3, 5), 15, 0, 1, true)
    wallR.restitution = 1.0
    worldAddBody(world, wallR)

    resetRandom()
    for i = 1, 30 do
        local radius = randomRange(0.3, 0.7)
        local x = randomRange(-12, 12)
        local y = randomRange(-3, 3)
        local ball = createBody(createCircle(radius), x, y, 2.0, false)
        ball.restitution = 0.98
        ball.linearDamping = 0.0
        ball.dynamicFriction = 0.0
        ball.velocity = vec(randomRange(-5, 5), randomRange(-5, 5))
        worldAddBody(world, ball)
    end

    return world
end

-- ============================================================================
-- Material property tables (realistic physical properties)
-- ============================================================================

local materials = {
    steel = {density = 7.8, restitution = 0.6, staticFriction = 0.74, dynamicFriction = 0.57},
    aluminum = {density = 2.7, restitution = 0.7, staticFriction = 0.61, dynamicFriction = 0.47},
    wood_oak = {density = 0.6, restitution = 0.4, staticFriction = 0.62, dynamicFriction = 0.48},
    wood_pine = {density = 0.4, restitution = 0.3, staticFriction = 0.56, dynamicFriction = 0.42},
    rubber = {density = 1.1, restitution = 0.85, staticFriction = 1.0, dynamicFriction = 0.8},
    ice = {density = 0.92, restitution = 0.3, staticFriction = 0.1, dynamicFriction = 0.03},
    concrete = {density = 2.4, restitution = 0.2, staticFriction = 0.75, dynamicFriction = 0.6},
    glass = {density = 2.5, restitution = 0.65, staticFriction = 0.94, dynamicFriction = 0.4},
    plastic = {density = 1.2, restitution = 0.5, staticFriction = 0.4, dynamicFriction = 0.3},
    leather = {density = 0.86, restitution = 0.35, staticFriction = 0.6, dynamicFriction = 0.48},
    cork = {density = 0.12, restitution = 0.6, staticFriction = 0.5, dynamicFriction = 0.4},
    titanium = {density = 4.5, restitution = 0.55, staticFriction = 0.36, dynamicFriction = 0.3},
    copper = {density = 8.9, restitution = 0.4, staticFriction = 0.53, dynamicFriction = 0.36},
    lead = {density = 11.3, restitution = 0.15, staticFriction = 0.43, dynamicFriction = 0.3},
    teflon = {density = 2.2, restitution = 0.3, staticFriction = 0.04, dynamicFriction = 0.04},
    sandstone = {density = 2.3, restitution = 0.15, staticFriction = 0.7, dynamicFriction = 0.55},
    marble = {density = 2.7, restitution = 0.5, staticFriction = 0.6, dynamicFriction = 0.4},
    granite = {density = 2.75, restitution = 0.25, staticFriction = 0.65, dynamicFriction = 0.5},
    bone = {density = 1.9, restitution = 0.35, staticFriction = 0.45, dynamicFriction = 0.3},
    cartilage = {density = 1.1, restitution = 0.7, staticFriction = 0.03, dynamicFriction = 0.02},
}

local function applyMaterial(body, materialName)
    local mat = materials[materialName]
    if not mat then return end
    body.restitution = mat.restitution
    body.staticFriction = mat.staticFriction
    body.dynamicFriction = mat.dynamicFriction
end

-- ============================================================================
-- Scenario 31: Material interaction test
-- ============================================================================

function createMaterialTestScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(25, 0.5), 0, -0.5, 1, true)
    applyMaterial(ground, "concrete")
    worldAddBody(world, ground)

    local ramp = createBody(createBox(8, 0.2), 0, 5, 1, true)
    ramp.angle = -0.3
    applyMaterial(ramp, "ice")
    worldAddBody(world, ramp)

    local materialNames = {"steel", "rubber", "wood_oak", "ice", "glass", "plastic",
                           "cork", "leather", "teflon", "copper"}

    for i = 1, #materialNames do
        local mat = materials[materialNames[i]]
        local x = -6 + (i - 1) * 1.2
        local body = createBody(createBox(0.4, 0.4), x, 7, mat.density, false)
        applyMaterial(body, materialNames[i])
        worldAddBody(world, body)
    end

    return world
end

-- ============================================================================
-- Pre-defined complex polygon shapes for testing
-- ============================================================================

local complexShapes = {
    star = function()
        local verts = {}
        for i = 1, 10 do
            local angle = (i - 1) * math_pi / 5 - math_pi / 2
            local r = (i % 2 == 1) and 1.0 or 0.4
            verts[i] = vec(r * math_cos(angle), r * math_sin(angle))
        end
        return computeConvexHull(verts)
    end,
    arrow = function()
        return {
            vec(0, 1.5), vec(0.8, 0.5), vec(0.3, 0.5),
            vec(0.3, -1.5), vec(-0.3, -1.5), vec(-0.3, 0.5), vec(-0.8, 0.5)
        }
    end,
    diamond = function()
        return {vec(0, 1.2), vec(0.8, 0), vec(0, -1.2), vec(-0.8, 0)}
    end,
    trapezoid = function()
        return {vec(-0.5, 0.5), vec(0.5, 0.5), vec(1.0, -0.5), vec(-1.0, -0.5)}
    end,
    lshape = function()
        return computeConvexHull({
            vec(-0.5, 1.0), vec(0.0, 1.0), vec(0.0, 0.0),
            vec(1.0, 0.0), vec(1.0, -0.5), vec(-0.5, -0.5)
        })
    end,
    chevron = function()
        return computeConvexHull({
            vec(0, 1.0), vec(0.6, 0.3), vec(0.6, -0.3),
            vec(0, -1.0), vec(-0.6, -0.3), vec(-0.6, 0.3)
        })
    end,
    cross = function()
        return computeConvexHull({
            vec(-0.3, 1.0), vec(0.3, 1.0), vec(0.3, 0.3),
            vec(1.0, 0.3), vec(1.0, -0.3), vec(0.3, -0.3),
            vec(0.3, -1.0), vec(-0.3, -1.0), vec(-0.3, -0.3),
            vec(-1.0, -0.3), vec(-1.0, 0.3), vec(-0.3, 0.3)
        })
    end,
    kite = function()
        return {vec(0, 1.5), vec(0.7, 0.2), vec(0, -0.8), vec(-0.7, 0.2)}
    end,
    parallelogram = function()
        return {vec(-0.3, 0.5), vec(0.7, 0.5), vec(0.3, -0.5), vec(-0.7, -0.5)}
    end,
    shield = function()
        return computeConvexHull({
            vec(-0.8, 0.8), vec(0.8, 0.8), vec(1.0, 0.0),
            vec(0.5, -0.8), vec(0, -1.2), vec(-0.5, -0.8), vec(-1.0, 0.0)
        })
    end
}

-- ============================================================================
-- Scenario 32: Complex polygon collisions
-- ============================================================================

function createComplexPolygonScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(20, 0.5), 0, -0.5, 1, true)
    worldAddBody(world, ground)

    local shapeNames = {"star", "arrow", "diamond", "trapezoid", "lshape",
                        "chevron", "cross", "kite", "parallelogram", "shield"}

    resetRandom()
    for i = 1, #shapeNames do
        local verts = complexShapes[shapeNames[i]]()
        local shape = createPolygon(verts)
        local x = -8 + (i - 1) * 1.8
        local y = randomRange(5, 12)
        local body = createBody(shape, x, y, 2.0, false)
        body.angle = randomRange(0, math_pi)
        body.restitution = 0.3
        worldAddBody(world, body)
    end

    for i = 1, 5 do
        local verts = complexShapes[shapeNames[i]]()
        local shape = createPolygon(verts)
        local x = randomRange(-6, 6)
        local body = createBody(shape, x, 15 + i, 3.0, false)
        body.velocity = vec(randomRange(-3, 3), -5)
        body.angularVelocity = randomRange(-2, 2)
        worldAddBody(world, body)
    end

    return world
end

-- ============================================================================
-- Continuous rotation angle normalization and angular limit helper
-- ============================================================================

local function normalizeAngle(angle)
    while angle > math_pi do angle = angle - 2 * math_pi end
    while angle < -math_pi do angle = angle + 2 * math_pi end
    return angle
end

local function clampAngularVelocity(body, maxOmega)
    if body.angularVelocity > maxOmega then
        body.angularVelocity = maxOmega
    elseif body.angularVelocity < -maxOmega then
        body.angularVelocity = -maxOmega
    end
end

-- ============================================================================
-- Position correction (separate pass for penetration resolution)
-- ============================================================================

function solvePositionConstraints(manifolds, bodies)
    local slop = 0.005
    local maxCorrection = 0.2
    local baumgarte = 0.4
    local corrected = false

    for i = 1, #manifolds do
        local m = manifolds[i]
        local bodyA = m.bodyA
        local bodyB = m.bodyB

        if m.penetration > slop then
            local correction = math_min((m.penetration - slop) * baumgarte, maxCorrection)
            local totalInvMass = bodyA.invMass + bodyB.invMass
            if totalInvMass > 0 then
                local moveA = correction * bodyA.invMass / totalInvMass
                local moveB = correction * bodyB.invMass / totalInvMass
                if not bodyA.isStatic then
                    bodyA.position = vecSub(bodyA.position, vecMul(m.normal, moveA))
                end
                if not bodyB.isStatic then
                    bodyB.position = vecAdd(bodyB.position, vecMul(m.normal, moveB))
                end
                corrected = true
            end
        end
    end

    return corrected
end

-- ============================================================================
-- Warm starting (cache impulses between frames)
-- ============================================================================

local warmStartCache = {}

local function getWarmStartKey(idA, idB)
    if idA < idB then return idA * 100000 + idB end
    return idB * 100000 + idA
end

local function applyWarmStart(manifold)
    local key = getWarmStartKey(manifold.bodyA.id, manifold.bodyB.id)
    local cached = warmStartCache[key]
    if not cached then return end

    local bodyA = manifold.bodyA
    local bodyB = manifold.bodyB
    local normal = manifold.normal

    for i = 1, math_min(#manifold.contacts, #cached) do
        local cp = manifold.contacts[i]
        local prev = cached[i]
        if cp.rA and prev.normalImpulse then
            local impulse = vecMul(normal, prev.normalImpulse * 0.8)
            bodyA.velocity = vecSub(bodyA.velocity, vecMul(impulse, bodyA.invMass))
            bodyA.angularVelocity = bodyA.angularVelocity - bodyA.invInertia * vecCross(cp.rA, impulse)
            bodyB.velocity = vecAdd(bodyB.velocity, vecMul(impulse, bodyB.invMass))
            bodyB.angularVelocity = bodyB.angularVelocity + bodyB.invInertia * vecCross(cp.rB, impulse)
        end
    end
end

local function saveWarmStart(manifold)
    local key = getWarmStartKey(manifold.bodyA.id, manifold.bodyB.id)
    local data = {}
    for i = 1, #manifold.contacts do
        local cp = manifold.contacts[i]
        data[i] = {normalImpulse = cp.normalImpulse, tangentImpulse = cp.tangentImpulse}
    end
    warmStartCache[key] = data
end

-- ============================================================================
-- Scenario 33: Large-scale stress test (many bodies, many contacts)
-- ============================================================================

function createStressTestScenario()
    local world = createWorld(vec(0, -10), 2.0)
    world.iterations = 8

    local ground = createBody(createBox(30, 0.5), 0, -0.5, 1, true)
    ground.staticFriction = 0.8
    worldAddBody(world, ground)

    local wallL = createBody(createBox(0.3, 15), -10, 7.5, 1, true)
    worldAddBody(world, wallL)
    local wallR = createBody(createBox(0.3, 15), 10, 7.5, 1, true)
    worldAddBody(world, wallR)

    resetRandom()
    for i = 1, 100 do
        local x = randomRange(-9, 9)
        local y = randomRange(1, 25)
        local shapeChoice = math_floor(random() * 4)
        local body
        if shapeChoice == 0 then
            body = createBody(createCircle(randomRange(0.2, 0.5)), x, y, 2.0, false)
        elseif shapeChoice == 1 then
            body = createBody(createBox(randomRange(0.2, 0.6), randomRange(0.2, 0.6)), x, y, 2.0, false)
        elseif shapeChoice == 2 then
            body = createBody(createRegularPolygon(randomRange(0.2, 0.5), 5), x, y, 2.0, false)
        else
            body = createBody(createRegularPolygon(randomRange(0.2, 0.5), 6), x, y, 2.0, false)
        end
        body.restitution = randomRange(0.0, 0.4)
        body.dynamicFriction = randomRange(0.3, 0.7)
        body.angle = randomRange(0, math_pi * 2)
        worldAddBody(world, body)
    end

    return world
end

-- ============================================================================
-- Scenario 34: Castle structure (detailed brick placement)
-- ============================================================================

function createCastleScenario()
    local world = createWorld(vec(0, -10), 2.5)

    local ground = createBody(createBox(40, 1), 0, -1, 1, true)
    ground.staticFriction = 0.9
    worldAddBody(world, ground)

    local brickW = 0.6
    local brickH = 0.3
    local mortar = 0.02

    local function placeBrick(x, y, w, h, density, isStatic)
        w = w or brickW
        h = h or brickH
        density = density or 3.0
        isStatic = isStatic or false
        local b = createBody(createBox(w, h), x, y, density, isStatic)
        b.restitution = 0.0
        b.staticFriction = 0.75
        b.dynamicFriction = 0.6
        worldAddBody(world, b)
        return b
    end

    local towerX = -12
    local towerWidth = 4
    local towerHeight = 12
    local brickPerRow = math_floor(towerWidth / (brickW * 2 + mortar))

    for row = 0, towerHeight - 1 do
        local y = 0.3 + row * (brickH * 2 + mortar)
        local offset = (row % 2 == 0) and 0 or (brickW + mortar / 2)
        for col = 0, brickPerRow do
            local x = towerX - towerWidth / 2 + offset + col * (brickW * 2 + mortar)
            if x >= towerX - towerWidth / 2 and x <= towerX + towerWidth / 2 then
                placeBrick(x, y)
            end
        end
    end

    for row = 0, 3 do
        local y = 0.3 + towerHeight * (brickH * 2 + mortar) + row * (brickH * 2 + mortar)
        for col = 0, brickPerRow + 1 do
            local x = towerX - towerWidth / 2 - brickW + col * (brickW * 2 + mortar)
            if col % 2 == 0 or row < 2 then
                placeBrick(x, y)
            end
        end
    end

    local tower2X = 12
    for row = 0, towerHeight - 1 do
        local y = 0.3 + row * (brickH * 2 + mortar)
        local offset = (row % 2 == 0) and 0 or (brickW + mortar / 2)
        for col = 0, brickPerRow do
            local x = tower2X - towerWidth / 2 + offset + col * (brickW * 2 + mortar)
            if x >= tower2X - towerWidth / 2 and x <= tower2X + towerWidth / 2 then
                placeBrick(x, y)
            end
        end
    end

    for row = 0, 3 do
        local y = 0.3 + towerHeight * (brickH * 2 + mortar) + row * (brickH * 2 + mortar)
        for col = 0, brickPerRow + 1 do
            local x = tower2X - towerWidth / 2 - brickW + col * (brickW * 2 + mortar)
            if col % 2 == 0 or row < 2 then
                placeBrick(x, y)
            end
        end
    end

    local wallStartX = towerX + towerWidth / 2 + brickW
    local wallEndX = tower2X - towerWidth / 2 - brickW
    local wallHeight = 8
    local wallBricksPerRow = math_floor((wallEndX - wallStartX) / (brickW * 2 + mortar))
    for row = 0, wallHeight - 1 do
        local y = 0.3 + row * (brickH * 2 + mortar)
        local offset = (row % 2 == 0) and 0 or (brickW + mortar / 2)
        for col = 0, wallBricksPerRow do
            local x = wallStartX + offset + col * (brickW * 2 + mortar)
            if x <= wallEndX then
                placeBrick(x, y)
            end
        end
    end

    local gateX = (towerX + tower2X) / 2
    local gateWidth = 3
    local gateHeight = 4
    local archHeight = wallHeight
    for row = gateHeight, archHeight do
        local y = 0.3 + row * (brickH * 2 + mortar)
        local rowWidth = gateWidth * (1 - (row - gateHeight) / (archHeight - gateHeight + 1) * 0.3)
        local numBricks = math_floor(rowWidth / (brickW * 2 + mortar)) + 1
        for col = 0, numBricks do
            local x = gateX - rowWidth / 2 + col * (brickW * 2 + mortar)
            placeBrick(x, y, brickW * 0.8, brickH * 0.8)
        end
    end

    local cannonball = createBody(createCircle(0.8), -20, 5, 15.0, false)
    cannonball.velocity = vec(20, 3)
    cannonball.restitution = 0.1
    worldAddBody(world, cannonball)

    return world
end

-- ============================================================================
-- Scenario 35: Clockwork mechanism (many gears and linkages)
-- ============================================================================

function createClockworkScenario()
    local world = createWorld(vec(0, 0), 4.0)
    world.gravity = vec(0, 0)

    local gears = {}
    local pivots = {}
    local joints = {}

    local gearLayout = {
        {x = 0, y = 0, r = 2.0, teeth = 20, speed = 1.0},
        {x = 3.5, y = 0, r = 1.5, teeth = 15, speed = -1.33},
        {x = 3.5, y = 3.0, r = 1.0, teeth = 10, speed = 2.0},
        {x = 6.0, y = 0, r = 1.2, teeth = 12, speed = 1.67},
        {x = 6.0, y = -2.5, r = 0.8, teeth = 8, speed = -2.5},
        {x = 0, y = -3.5, r = 1.8, teeth = 18, speed = -1.11},
        {x = -3.0, y = -2.0, r = 1.0, teeth = 10, speed = 2.0},
        {x = -3.0, y = 1.5, r = 1.3, teeth = 13, speed = -1.54},
        {x = -5.5, y = 0, r = 0.9, teeth = 9, speed = 2.22},
        {x = 0, y = 4.0, r = 1.6, teeth = 16, speed = -1.25},
        {x = -2.5, y = 4.5, r = 0.7, teeth = 7, speed = 2.86},
        {x = 2.5, y = 4.0, r = 1.1, teeth = 11, speed = 1.82},
    }

    for i = 1, #gearLayout do
        local gl = gearLayout[i]
        local gear = createBody(createRegularPolygon(gl.r, gl.teeth), gl.x, gl.y, 3.0, false)
        gear.angularDamping = 0.01
        gear.linearDamping = 10
        worldAddBody(world, gear)
        gears[i] = gear

        local pivot = createBody(createCircle(0.1), gl.x, gl.y, 1, true)
        worldAddBody(world, pivot)
        pivots[i] = pivot

        local joint = createRevoluteJoint(pivot, gear, vec(0, 0), vec(0, 0))
        if i == 1 then
            joint.motorEnabled = true
            joint.motorSpeed = gl.speed * 3
            joint.maxMotorTorque = 200
        end
        worldAddJoint(world, joint)
        joints[i] = joint
    end

    local gearConnections = {
        {1, 2}, {2, 3}, {2, 4}, {4, 5}, {1, 6}, {6, 7}, {1, 8}, {8, 9},
        {1, 10}, {10, 11}, {10, 12}
    }

    for i = 1, #gearConnections do
        local conn = gearConnections[i]
        local a = conn[1]
        local b = conn[2]
        local ratio = -gearLayout[a].r / gearLayout[b].r
        local gj = createGearJoint(joints[a], joints[b], ratio)
        worldAddJoint(world, gj)
    end

    local crankGear = gears[5]
    local crankLength = 2.0
    local crankArm = createBody(createBox(crankLength / 2, 0.1), gearLayout[5].x + crankLength / 2, gearLayout[5].y, 1.5, false)
    worldAddBody(world, crankArm)
    local crankJoint = createRevoluteJoint(crankGear, crankArm, vec(0.6, 0), vec(-crankLength / 2, 0))
    worldAddJoint(world, crankJoint)

    local piston = createBody(createBox(0.3, 0.5), gearLayout[5].x + crankLength + 1, gearLayout[5].y, 2.0, false)
    worldAddBody(world, piston)
    local pistonJoint = createRevoluteJoint(crankArm, piston, vec(crankLength / 2, 0), vec(0, 0))
    worldAddJoint(world, pistonJoint)

    local guide = createBody(createBox(0.1, 2), gearLayout[5].x + crankLength + 1, gearLayout[5].y, 1, true)
    worldAddBody(world, guide)
    local slideJoint = createPrismaticJoint(guide, piston, vec(0, 0), vec(0, 0), vec(0, 1))
    worldAddJoint(world, slideJoint)

    local escapementWheel = createBody(createRegularPolygon(1.5, 15), -6, -4, 4.0, false)
    escapementWheel.angularDamping = 0.01
    worldAddBody(world, escapementWheel)
    local escPivot = createBody(createCircle(0.1), -6, -4, 1, true)
    worldAddBody(world, escPivot)
    local escJoint = createRevoluteJoint(escPivot, escapementWheel, vec(0, 0), vec(0, 0))
    escJoint.motorEnabled = true
    escJoint.motorSpeed = 0.5
    escJoint.maxMotorTorque = 10
    worldAddJoint(world, escJoint)

    local pendulumLength = 4
    local pendulumBob = createBody(createCircle(0.4), -6, -4 - pendulumLength, 5.0, false)
    worldAddBody(world, pendulumBob)
    local pendJoint = createDistanceJoint(escPivot, pendulumBob, vec(0, 0), vec(0, 0), pendulumLength)
    pendJoint.stiffness = 500
    pendJoint.damping = 0.5
    worldAddJoint(world, pendJoint)

    pendulumBob.position = vec(-6 + 1.5, -4 - pendulumLength + 0.5)

    return world
end

-- ============================================================================
-- Scenario 36: Trebuchet with projectile arc
-- ============================================================================

function createTrebuchetScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(40, 0.5), 0, -0.5, 1, true)
    worldAddBody(world, ground)

    local baseX = -15
    local baseY = 0

    local frameLeft = createBody(createBox(0.2, 3), baseX - 1.5, baseY + 3, 1, true)
    worldAddBody(world, frameLeft)
    local frameRight = createBody(createBox(0.2, 3), baseX + 1.5, baseY + 3, 1, true)
    worldAddBody(world, frameRight)
    local frameTop = createBody(createBox(2, 0.2), baseX, baseY + 6.2, 1, true)
    worldAddBody(world, frameTop)

    local armLength = 8
    local armPivotRatio = 0.3
    local arm = createBody(createBox(armLength / 2, 0.15), baseX, baseY + 6, 3.0, false)
    worldAddBody(world, arm)

    local armPivot = createRevoluteJoint(frameTop, arm, vec(0, 0),
        vec(-armLength / 2 + armLength * armPivotRatio, 0))
    worldAddJoint(world, armPivot)

    local counterweightMass = 30
    local cwX = baseX - armLength * (1 - armPivotRatio) + armLength * armPivotRatio
    local counterweight = createBody(createBox(0.8, 0.8), cwX, baseY + 5, counterweightMass, false)
    worldAddBody(world, counterweight)
    local cwRope = createDistanceJoint(arm, counterweight,
        vec(-armLength / 2 + armLength * armPivotRatio - 1, 0), vec(0, 0.4), 1.0)
    cwRope.stiffness = 500
    cwRope.damping = 5
    worldAddJoint(world, cwRope)

    local projX = baseX + armLength * (1 - armPivotRatio) - 0.5
    local projectile = createBody(createCircle(0.3), projX, baseY + 1, 2.0, false)
    projectile.restitution = 0.3
    worldAddBody(world, projectile)

    local slingLength = 3
    local slingJoint = createRopeJoint(arm, projectile,
        vec(armLength / 2 - armLength * armPivotRatio, 0), vec(0, 0), slingLength)
    worldAddJoint(world, slingJoint)

    arm.angle = 0.5
    arm.angularVelocity = -2

    local targetX = 15
    for row = 0, 5 do
        for col = 0, 4 do
            local x = targetX + col * 0.7
            local y = 0.25 + row * 0.5
            local target = createBody(createBox(0.3, 0.2), x, y, 1.5, false)
            target.restitution = 0.05
            target.staticFriction = 0.6
            worldAddBody(world, target)
        end
    end

    return world
end

-- ============================================================================
-- Scenario 37: Fluid-like particle simulation (SPH-inspired)
-- ============================================================================

function createFluidScenario()
    local world = createWorld(vec(0, -10), 1.5)

    local containerW = 8
    local containerH = 10

    local bottom = createBody(createBox(containerW / 2, 0.3), 0, -0.3, 1, true)
    worldAddBody(world, bottom)
    local leftW = createBody(createBox(0.3, containerH / 2), -containerW / 2 - 0.3, containerH / 2, 1, true)
    worldAddBody(world, leftW)
    local rightW = createBody(createBox(0.3, containerH / 2), containerW / 2 + 0.3, containerH / 2, 1, true)
    worldAddBody(world, rightW)

    local obstacleVerts = {vec(-1.5, -0.3), vec(1.5, 0.3), vec(1.5, -0.3)}
    local obstacle = createBody(createPolygon(obstacleVerts), 0, 5, 1, true)
    worldAddBody(world, obstacle)

    local particleRadius = 0.2
    local particleSpacing = particleRadius * 2.2
    local startX = -containerW / 2 + 1
    local startY = 7

    resetRandom()
    for row = 0, 11 do
        for col = 0, 11 do
            local x = startX + col * particleSpacing + randomRange(-0.02, 0.02)
            local y = startY + row * particleSpacing + randomRange(-0.02, 0.02)
            local p = createBody(createCircle(particleRadius), x, y, 1.0, false)
            p.restitution = 0.0
            p.dynamicFriction = 0.1
            p.linearDamping = 0.3
            worldAddBody(world, p)
        end
    end

    return world
end

-- ============================================================================
-- Scenario 38: Windmill with blades and falling objects
-- ============================================================================

function createWindmillScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(20, 0.5), 0, -0.5, 1, true)
    worldAddBody(world, ground)

    local towerBase = createBody(createBox(1.5, 4), 0, 4, 1, true)
    worldAddBody(world, towerBase)

    local hubX = 0
    local hubY = 9
    local hub = createBody(createCircle(0.3), hubX, hubY, 5.0, false)
    hub.angularDamping = 0.02
    worldAddBody(world, hub)

    local hubPivot = createBody(createCircle(0.1), hubX, hubY, 1, true)
    worldAddBody(world, hubPivot)
    local hubJoint = createRevoluteJoint(hubPivot, hub, vec(0, 0), vec(0, 0))
    hubJoint.motorEnabled = true
    hubJoint.motorSpeed = 3
    hubJoint.maxMotorTorque = 50
    worldAddJoint(world, hubJoint)

    local numBlades = 4
    local bladeLength = 3.5
    local bladeWidth = 0.15
    for i = 1, numBlades do
        local angle = (i - 1) * math_pi * 2 / numBlades
        local bladeX = hubX + (bladeLength / 2 + 0.3) * math_cos(angle)
        local bladeY = hubY + (bladeLength / 2 + 0.3) * math_sin(angle)
        local blade = createBody(createBox(bladeLength / 2, bladeWidth), bladeX, bladeY, 2.0, false)
        blade.angle = angle
        worldAddBody(world, blade)

        local wj = createWeldJoint(hub, blade,
            vec(0.3 * math_cos(angle), 0.3 * math_sin(angle)),
            vec(-bladeLength / 2, 0))
        worldAddJoint(world, wj)
    end

    resetRandom()
    for i = 1, 20 do
        local x = randomRange(-8, 8)
        local y = randomRange(14, 22)
        local sc = math_floor(random() * 3)
        local body
        if sc == 0 then
            body = createBody(createCircle(randomRange(0.2, 0.4)), x, y, 2.0, false)
        elseif sc == 1 then
            body = createBody(createBox(randomRange(0.2, 0.5), randomRange(0.2, 0.5)), x, y, 2.0, false)
        else
            body = createBody(createRegularPolygon(randomRange(0.2, 0.4), 5), x, y, 2.0, false)
        end
        body.restitution = 0.3
        worldAddBody(world, body)
    end

    return world
end

-- ============================================================================
-- Scenario 39: Multi-body vehicle (car with suspension)
-- ============================================================================

function createDetailedVehicleScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local terrainSegs = {
        {x = -20, y = 0}, {x = -15, y = 0}, {x = -10, y = 0.5}, {x = -5, y = 0.3},
        {x = 0, y = 0}, {x = 5, y = -0.2}, {x = 8, y = 0.5}, {x = 10, y = 1.5},
        {x = 12, y = 2.0}, {x = 14, y = 1.8}, {x = 16, y = 1.0}, {x = 18, y = 0.5},
        {x = 20, y = 0}, {x = 25, y = 0},
    }

    for i = 1, #terrainSegs - 1 do
        local p1 = terrainSegs[i]
        local p2 = terrainSegs[i + 1]
        local midX = (p1.x + p2.x) / 2
        local midY = (p1.y + p2.y) / 2
        local dx = p2.x - p1.x
        local dy = p2.y - p1.y
        local len = math_sqrt(dx * dx + dy * dy)
        local seg = createBody(createBox(len / 2, 0.3), midX, midY - 0.3, 1, true)
        seg.angle = math_atan2(dy, dx)
        seg.staticFriction = 0.9
        worldAddBody(world, seg)
    end

    local carX = -18
    local carY = 2

    local chassis = createBody(createPolygon({
        vec(-2.0, -0.3), vec(-1.8, 0.3), vec(-0.5, 0.5),
        vec(1.5, 0.5), vec(2.0, 0.2), vec(2.0, -0.3)
    }), carX, carY, 4.0, false)
    chassis.linearDamping = 0.05
    worldAddBody(world, chassis)

    local fenderFront = createBody(createBox(0.6, 0.15), carX + 1.8, carY - 0.1, 1.0, false)
    worldAddBody(world, fenderFront)
    local fwj = createWeldJoint(chassis, fenderFront, vec(1.8, -0.1), vec(0, 0))
    worldAddJoint(world, fwj)

    local fenderRear = createBody(createBox(0.6, 0.15), carX - 1.6, carY - 0.1, 1.0, false)
    worldAddBody(world, fenderRear)
    local rwj = createWeldJoint(chassis, fenderRear, vec(-1.6, -0.1), vec(0, 0))
    worldAddJoint(world, rwj)

    local wheelR = 0.45
    local wheelDensity = 3.0

    local frontWheel = createBody(createCircle(wheelR), carX + 1.5, carY - 0.8, wheelDensity, false)
    frontWheel.dynamicFriction = 0.9
    frontWheel.restitution = 0.1
    worldAddBody(world, frontWheel)

    local rearWheel = createBody(createCircle(wheelR), carX - 1.5, carY - 0.8, wheelDensity, false)
    rearWheel.dynamicFriction = 0.9
    rearWheel.restitution = 0.1
    worldAddBody(world, rearWheel)

    local fwJoint = createWheelJoint(chassis, frontWheel,
        vec(1.5, -0.5), vec(0, 0), vec(0, 1))
    fwJoint.springStiffness = 100
    fwJoint.springDamping = 10
    worldAddJoint(world, fwJoint)

    local rwJoint = createWheelJoint(chassis, rearWheel,
        vec(-1.5, -0.5), vec(0, 0), vec(0, 1))
    rwJoint.springStiffness = 100
    rwJoint.springDamping = 10
    rwJoint.motorEnabled = true
    rwJoint.motorSpeed = -20
    rwJoint.maxMotorTorque = 80
    worldAddJoint(world, rwJoint)

    return world
end

-- ============================================================================
-- Scenario 40: Bowling alley
-- ============================================================================

function createBowlingScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local laneLength = 25
    local laneWidth = 3
    local lane = createBody(createBox(laneLength / 2, 0.3), 0, -0.3, 1, true)
    lane.staticFriction = 0.2
    lane.dynamicFriction = 0.1
    worldAddBody(world, lane)

    local gutterL = createBody(createBox(laneLength / 2, 0.15), 0, 0, 1, true)
    gutterL.angle = 0
    worldAddBody(world, gutterL)

    local backwall = createBody(createBox(laneWidth, 0.3), laneLength / 2 - 0.5, 1, 1, true)
    backwall.restitution = 0.3
    worldAddBody(world, backwall)

    local pinRadius = 0.15
    local pinHeight = 0.5
    local pinDensity = 2.0
    local pinSpacing = pinRadius * 3.5
    local pinStartX = laneLength / 2 - 3
    local pinStartY = 0.5

    local pinPositions = {}
    for row = 0, 3 do
        for col = 0, row do
            local x = pinStartX + row * pinSpacing * 0.866
            local y = pinStartY + (col - row / 2) * pinSpacing
            pinPositions[#pinPositions + 1] = {x = x, y = y}
        end
    end

    for i = 1, #pinPositions do
        local pp = pinPositions[i]
        local pin = createBody(createBox(pinRadius, pinHeight / 2), pp.x, pp.y + pinHeight / 2, pinDensity, false)
        pin.restitution = 0.3
        pin.staticFriction = 0.5
        worldAddBody(world, pin)
    end

    local ballRadius = 0.35
    local ball = createBody(createCircle(ballRadius), -laneLength / 2 + 2, 0.35, 7.0, false)
    ball.velocity = vec(12, 0.3)
    ball.angularVelocity = -5
    ball.restitution = 0.2
    ball.dynamicFriction = 0.05
    worldAddBody(world, ball)

    return world
end

-- ============================================================================
-- Scenario 41: Earthquake simulation (shaking ground)
-- ============================================================================

function createEarthquakeScenario()
    local world = createWorld(vec(0, -10), 2.5)

    local ground = createBody(createBox(25, 0.5), 0, -0.5, 1, true)
    ground.staticFriction = 0.7
    worldAddBody(world, ground)

    local buildingX = -8
    local buildingFloors = 6
    local buildingWidth = 4
    local floorHeight = 1.2
    local columnWidth = 0.2
    local columnHeight = floorHeight / 2 - 0.1

    for floor = 0, buildingFloors - 1 do
        local baseY = floor * floorHeight + 0.5

        local leftCol = createBody(createBox(columnWidth, columnHeight),
            buildingX - buildingWidth / 2 + columnWidth, baseY + columnHeight, 4.0, false)
        leftCol.staticFriction = 0.6
        worldAddBody(world, leftCol)

        local rightCol = createBody(createBox(columnWidth, columnHeight),
            buildingX + buildingWidth / 2 - columnWidth, baseY + columnHeight, 4.0, false)
        rightCol.staticFriction = 0.6
        worldAddBody(world, rightCol)

        local midCol = createBody(createBox(columnWidth, columnHeight),
            buildingX, baseY + columnHeight, 4.0, false)
        midCol.staticFriction = 0.6
        worldAddBody(world, midCol)

        local slab = createBody(createBox(buildingWidth / 2 + 0.2, 0.1),
            buildingX, baseY + floorHeight - 0.1, 5.0, false)
        slab.staticFriction = 0.6
        worldAddBody(world, slab)
    end

    local tower2X = 5
    local towerFloors = 8
    local towerWidth = 2.5

    for floor = 0, towerFloors - 1 do
        local baseY = floor * 1.0 + 0.5
        local leftCol = createBody(createBox(0.15, 0.4),
            tower2X - towerWidth / 2 + 0.15, baseY + 0.4, 4.0, false)
        leftCol.staticFriction = 0.6
        worldAddBody(world, leftCol)

        local rightCol = createBody(createBox(0.15, 0.4),
            tower2X + towerWidth / 2 - 0.15, baseY + 0.4, 4.0, false)
        rightCol.staticFriction = 0.6
        worldAddBody(world, rightCol)

        local slab = createBody(createBox(towerWidth / 2, 0.08),
            tower2X, baseY + 0.88, 3.0, false)
        slab.staticFriction = 0.6
        worldAddBody(world, slab)
    end

    return world
end

-- ============================================================================
-- Scenario 42: Pachinko machine (many pegs, falling balls)
-- ============================================================================

function createPachinkoScenario()
    local world = createWorld(vec(0, -8), 2.0)

    local boardW = 12
    local boardH = 18
    local pegRadius = 0.2
    local pegSpacing = 1.2

    local leftWall = createBody(createBox(0.3, boardH / 2), -boardW / 2 - 0.3, boardH / 2, 1, true)
    worldAddBody(world, leftWall)
    local rightWall = createBody(createBox(0.3, boardH / 2), boardW / 2 + 0.3, boardH / 2, 1, true)
    worldAddBody(world, rightWall)
    local bottom = createBody(createBox(boardW / 2, 0.3), 0, -0.3, 1, true)
    worldAddBody(world, bottom)

    local numRows = math_floor(boardH / pegSpacing) - 2
    for row = 0, numRows - 1 do
        local y = boardH - 2 - row * pegSpacing
        local numPegs = math_floor(boardW / pegSpacing) - 1
        local offset = (row % 2 == 0) and 0 or (pegSpacing / 2)
        for col = 0, numPegs - 1 do
            local x = -boardW / 2 + pegSpacing + offset + col * pegSpacing
            if x > -boardW / 2 + 0.5 and x < boardW / 2 - 0.5 then
                local peg = createBody(createCircle(pegRadius), x, y, 1, true)
                peg.restitution = 0.5
                worldAddBody(world, peg)
            end
        end
    end

    local numSlots = 8
    local slotWidth = boardW / numSlots
    for i = 1, numSlots - 1 do
        local x = -boardW / 2 + i * slotWidth
        local divider = createBody(createBox(0.1, 0.8), x, 0.8, 1, true)
        worldAddBody(world, divider)
    end

    resetRandom()
    local ballRadius = 0.25
    for i = 1, 15 do
        local x = randomRange(-boardW / 2 + 1, boardW / 2 - 1)
        local y = boardH + i * 0.6
        local ball = createBody(createCircle(ballRadius), x, y, 3.0, false)
        ball.restitution = 0.4
        ball.dynamicFriction = 0.1
        worldAddBody(world, ball)
    end

    return world
end

-- ============================================================================
-- Scenario 43: Spring lattice (many interconnected springs)
-- ============================================================================

function createSpringLatticeScenario()
    local world = createWorld(vec(0, -5), 2.0)

    local cols = 8
    local rows = 8
    local spacing = 1.2
    local startX = -(cols - 1) * spacing / 2
    local startY = 5

    local ground = createBody(createBox(15, 0.3), 0, -0.3, 1, true)
    worldAddBody(world, ground)

    local nodes = {}
    for r = 0, rows - 1 do
        nodes[r] = {}
        for c = 0, cols - 1 do
            local x = startX + c * spacing
            local y = startY + r * spacing
            local isFixed = (r == rows - 1) and (c == 0 or c == cols - 1)
            local node = createBody(createCircle(0.15), x, y, 1.5, isFixed)
            node.linearDamping = 0.2
            worldAddBody(world, node)
            nodes[r][c] = node
        end
    end

    for r = 0, rows - 1 do
        for c = 0, cols - 1 do
            if c < cols - 1 then
                local j = createDistanceJoint(nodes[r][c], nodes[r][c + 1],
                    vec(0, 0), vec(0, 0), spacing)
                j.stiffness = 80
                j.damping = 3
                worldAddJoint(world, j)
            end
            if r < rows - 1 then
                local j = createDistanceJoint(nodes[r][c], nodes[r + 1][c],
                    vec(0, 0), vec(0, 0), spacing)
                j.stiffness = 80
                j.damping = 3
                worldAddJoint(world, j)
            end
            if c < cols - 1 and r < rows - 1 then
                local diagDist = spacing * 1.414
                local j = createDistanceJoint(nodes[r][c], nodes[r + 1][c + 1],
                    vec(0, 0), vec(0, 0), diagDist)
                j.stiffness = 40
                j.damping = 2
                worldAddJoint(world, j)
            end
        end
    end

    local impactBall = createBody(createCircle(0.8), 0, startY + rows * spacing + 3, 10.0, false)
    impactBall.velocity = vec(0, -8)
    impactBall.restitution = 0.5
    worldAddBody(world, impactBall)

    return world
end

-- ============================================================================
-- Scenario 44: Cannon with multiple projectiles
-- ============================================================================

function createCannonScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(35, 0.5), 0, -0.5, 1, true)
    worldAddBody(world, ground)

    local targetWallX = 15
    local wallRows = 10
    local wallCols = 5
    for row = 0, wallRows - 1 do
        for col = 0, wallCols - 1 do
            local x = targetWallX + col * 0.65
            local y = 0.3 + row * 0.5
            local brick = createBody(createBox(0.3, 0.2), x, y, 2.5, false)
            brick.restitution = 0.05
            brick.staticFriction = 0.6
            worldAddBody(world, brick)
        end
    end

    local cannonX = -15
    local cannonY = 2
    local cannonAngle = 0.5

    resetRandom()
    local numProjectiles = 8
    for i = 1, numProjectiles do
        local speed = randomRange(18, 25)
        local angle = cannonAngle + randomRange(-0.1, 0.1)
        local delay = (i - 1) * 0.3
        local vx = speed * math_cos(angle)
        local vy = speed * math_sin(angle)
        local startX = cannonX + vx * delay
        local startY = cannonY + vy * delay - 0.5 * 10 * delay * delay

        local proj = createBody(createCircle(0.3), startX, startY, 8.0, false)
        proj.velocity = vec(vx, vy - 10 * delay)
        proj.restitution = 0.2
        worldAddBody(world, proj)
    end

    return world
end

-- ============================================================================
-- Scenario 45: Wrecking yard (heavy machinery + debris)
-- ============================================================================

function createWreckingYardScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(30, 0.5), 0, -0.5, 1, true)
    worldAddBody(world, ground)

    resetRandom()
    local debrisCount = 50
    for i = 1, debrisCount do
        local x = randomRange(-15, 15)
        local y = randomRange(0.5, 2)
        local sc = math_floor(random() * 4)
        local body
        if sc == 0 then
            body = createBody(createCircle(randomRange(0.1, 0.4)), x, y, randomRange(1, 5), false)
        elseif sc == 1 then
            body = createBody(createBox(randomRange(0.2, 0.8), randomRange(0.1, 0.4)), x, y, randomRange(1, 5), false)
        elseif sc == 2 then
            body = createBody(createRegularPolygon(randomRange(0.2, 0.5), 5), x, y, randomRange(1, 5), false)
        else
            body = createBody(createRegularPolygon(randomRange(0.2, 0.5), 3), x, y, randomRange(1, 5), false)
        end
        body.restitution = randomRange(0.0, 0.3)
        body.staticFriction = randomRange(0.4, 0.8)
        worldAddBody(world, body)
    end

    local craneX = 0
    local craneY = 15
    local craneBase = createBody(createBox(1, 0.5), craneX, craneY, 1, true)
    worldAddBody(world, craneBase)

    local numCableLinks = 6
    local linkLen = 1.5
    local prevLink = craneBase
    for i = 1, numCableLinks do
        local link = createBody(createBox(0.1, linkLen / 2 - 0.05),
            craneX, craneY - i * linkLen, 1.0, false)
        link.angularDamping = 0.2
        worldAddBody(world, link)
        local j = createRevoluteJoint(prevLink, link,
            vec(0, i == 1 and -0.5 or -linkLen / 2 + 0.05), vec(0, linkLen / 2 - 0.05))
        worldAddJoint(world, j)
        prevLink = link
    end

    local wreckingBall = createBody(createCircle(1.5), craneX, craneY - numCableLinks * linkLen - 1.5, 25.0, false)
    wreckingBall.restitution = 0.2
    worldAddBody(world, wreckingBall)
    local bj = createRevoluteJoint(prevLink, wreckingBall, vec(0, -linkLen / 2), vec(0, 0.5))
    worldAddJoint(world, bj)

    wreckingBall.velocity = vec(8, -5)

    return world
end

-- ============================================================================
-- Cubic Bezier Spline system (for path-based scenarios)
-- ============================================================================

local function bezierPoint(p0, p1, p2, p3, t)
    local u = 1 - t
    local uu = u * u
    local uuu = uu * u
    local tt = t * t
    local ttt = tt * t
    return vec(
        uuu * p0.x + 3 * uu * t * p1.x + 3 * u * tt * p2.x + ttt * p3.x,
        uuu * p0.y + 3 * uu * t * p1.y + 3 * u * tt * p2.y + ttt * p3.y
    )
end

local function bezierTangent(p0, p1, p2, p3, t)
    local u = 1 - t
    local uu = u * u
    local tt = t * t
    return vec(
        3 * uu * (p1.x - p0.x) + 6 * u * t * (p2.x - p1.x) + 3 * tt * (p3.x - p2.x),
        3 * uu * (p1.y - p0.y) + 6 * u * t * (p2.y - p1.y) + 3 * tt * (p3.y - p2.y)
    )
end

local function bezierLength(p0, p1, p2, p3, segments)
    segments = segments or 20
    local len = 0
    local prev = p0
    for i = 1, segments do
        local t = i / segments
        local curr = bezierPoint(p0, p1, p2, p3, t)
        len = len + vecDist(prev, curr)
        prev = curr
    end
    return len
end

local function createSpline(controlPoints)
    local spline = {
        points = controlPoints,
        numSegments = math_floor((#controlPoints - 1) / 3)
    }
    return spline
end

local function splinePointAt(spline, t)
    local seg = math_floor(t * spline.numSegments)
    if seg >= spline.numSegments then seg = spline.numSegments - 1 end
    local localT = t * spline.numSegments - seg
    local base = seg * 3 + 1
    return bezierPoint(
        spline.points[base], spline.points[base + 1],
        spline.points[base + 2], spline.points[base + 3], localT)
end

local function splineTangentAt(spline, t)
    local seg = math_floor(t * spline.numSegments)
    if seg >= spline.numSegments then seg = spline.numSegments - 1 end
    local localT = t * spline.numSegments - seg
    local base = seg * 3 + 1
    return vecNormalize(bezierTangent(
        spline.points[base], spline.points[base + 1],
        spline.points[base + 2], spline.points[base + 3], localT))
end

-- ============================================================================
-- Predefined track splines for scenarios
-- ============================================================================

local trackSplines = {
    oval = createSpline({
        vec(-10, 0), vec(-10, 5), vec(-5, 8), vec(0, 8),
        vec(0, 8), vec(5, 8), vec(10, 5), vec(10, 0),
        vec(10, 0), vec(10, -5), vec(5, -8), vec(0, -8),
        vec(0, -8), vec(-5, -8), vec(-10, -5), vec(-10, 0),
    }),
    figure8 = createSpline({
        vec(0, 0), vec(3, 3), vec(6, 5), vec(8, 3),
        vec(8, 3), vec(10, 1), vec(8, -2), vec(5, -3),
        vec(5, -3), vec(2, -4), vec(-2, -4), vec(-5, -3),
        vec(-5, -3), vec(-8, -2), vec(-10, 1), vec(-8, 3),
        vec(-8, 3), vec(-6, 5), vec(-3, 3), vec(0, 0),
    }),
    roller = createSpline({
        vec(-15, 5), vec(-12, 5), vec(-10, 10), vec(-8, 10),
        vec(-8, 10), vec(-6, 10), vec(-4, 3), vec(-2, 3),
        vec(-2, 3), vec(0, 3), vec(2, 8), vec(4, 8),
        vec(4, 8), vec(6, 8), vec(8, 2), vec(10, 2),
        vec(10, 2), vec(12, 2), vec(14, 6), vec(15, 5),
    }),
}

-- ============================================================================
-- Scenario 46: Race track (bodies following spline path)
-- ============================================================================

function createRaceTrackScenario()
    local world = createWorld(vec(0, -10), 4.0)

    local spline = trackSplines.oval
    local numSegments = 40
    local trackWidth = 1.5

    for i = 0, numSegments - 1 do
        local t1 = i / numSegments
        local t2 = (i + 1) / numSegments
        local p1 = splinePointAt(spline, t1)
        local p2 = splinePointAt(spline, t2)
        local mid = vecLerp(p1, p2, 0.5)
        local dx = p2.x - p1.x
        local dy = p2.y - p1.y
        local len = math_sqrt(dx * dx + dy * dy)
        local angle = math_atan2(dy, dx)

        local seg = createBody(createBox(len / 2 + 0.1, 0.2), mid.x, mid.y, 1, true)
        seg.angle = angle
        seg.staticFriction = 0.9
        worldAddBody(world, seg)

        local tangent = vecNormalize(vec(dx, dy))
        local normal = vecPerp(tangent)
        local wallInner = createBody(createBox(len / 2, 0.1),
            mid.x - normal.x * trackWidth, mid.y - normal.y * trackWidth, 1, true)
        wallInner.angle = angle
        wallInner.restitution = 0.5
        worldAddBody(world, wallInner)

        local wallOuter = createBody(createBox(len / 2, 0.1),
            mid.x + normal.x * trackWidth, mid.y + normal.y * trackWidth, 1, true)
        wallOuter.angle = angle
        wallOuter.restitution = 0.5
        worldAddBody(world, wallOuter)
    end

    for i = 1, 4 do
        local t = (i - 1) * 0.25
        local pos = splinePointAt(spline, t)
        local car = createBody(createBox(0.6, 0.3), pos.x, pos.y + 0.5, 3.0, false)
        car.dynamicFriction = 0.4
        car.restitution = 0.3
        local tang = splineTangentAt(spline, t)
        car.velocity = vecMul(tang, 8 + i * 2)
        worldAddBody(world, car)
    end

    return world
end

-- ============================================================================
-- Scenario 47: Roller coaster track
-- ============================================================================

function createRollerCoasterScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local spline = trackSplines.roller
    local numRailSegs = 50

    for i = 0, numRailSegs - 1 do
        local t1 = i / numRailSegs
        local t2 = (i + 1) / numRailSegs
        local p1 = splinePointAt(spline, t1)
        local p2 = splinePointAt(spline, t2)
        local mid = vecLerp(p1, p2, 0.5)
        local dx = p2.x - p1.x
        local dy = p2.y - p1.y
        local len = math_sqrt(dx * dx + dy * dy)
        local angle = math_atan2(dy, dx)

        local rail = createBody(createBox(len / 2 + 0.05, 0.1), mid.x, mid.y, 1, true)
        rail.angle = angle
        rail.restitution = 0.1
        rail.staticFriction = 0.05
        worldAddBody(world, rail)
    end

    for i = 0, 9 do
        local t = i / 50
        local pos = splinePointAt(spline, t)
        local support = createBody(createBox(0.1, pos.y / 2), pos.x, pos.y / 2 - 0.5, 1, true)
        worldAddBody(world, support)
    end

    local ground = createBody(createBox(20, 0.3), 0, -0.8, 1, true)
    worldAddBody(world, ground)

    local startPos = splinePointAt(spline, 0)
    local cart = createBody(createBox(0.8, 0.3), startPos.x, startPos.y + 0.5, 5.0, false)
    cart.dynamicFriction = 0.02
    cart.restitution = 0.2
    local tang = splineTangentAt(spline, 0)
    cart.velocity = vecMul(tang, 12)
    worldAddBody(world, cart)

    return world
end

-- ============================================================================
-- Scenario 48: Destruction derby (cars crashing)
-- ============================================================================

function createDestructionDerbyScenario()
    local world = createWorld(vec(0, -10), 4.0)

    local arenaRadius = 12
    local numWallSegs = 24
    for i = 0, numWallSegs - 1 do
        local a1 = i * 2 * math_pi / numWallSegs
        local a2 = (i + 1) * 2 * math_pi / numWallSegs
        local p1 = vec(arenaRadius * math_cos(a1), arenaRadius * math_sin(a1))
        local p2 = vec(arenaRadius * math_cos(a2), arenaRadius * math_sin(a2))
        local mid = vecLerp(p1, p2, 0.5)
        local dx = p2.x - p1.x
        local dy = p2.y - p1.y
        local len = math_sqrt(dx * dx + dy * dy)
        local angle = math_atan2(dy, dx)
        local wall = createBody(createBox(len / 2, 0.4), mid.x, mid.y, 1, true)
        wall.angle = angle
        wall.restitution = 0.5
        worldAddBody(world, wall)
    end

    local ground = createBody(createBox(arenaRadius, 0.3), 0, -arenaRadius - 0.3, 1, true)
    worldAddBody(world, ground)

    local numCars = 8
    for i = 1, numCars do
        local angle = (i - 1) * 2 * math_pi / numCars
        local radius = 8
        local x = radius * math_cos(angle)
        local y = radius * math_sin(angle)

        local car = createBody(createBox(1.2, 0.5), x, y, 5.0, false)
        car.angle = angle + math_pi
        car.restitution = 0.4
        car.dynamicFriction = 0.5

        local speed = 10
        car.velocity = vec(-speed * math_cos(angle), -speed * math_sin(angle))
        worldAddBody(world, car)

        local frontBumper = createBody(createBox(0.15, 0.55), x + 1.3 * math_cos(angle + math_pi), y + 1.3 * math_sin(angle + math_pi), 3.0, false)
        frontBumper.restitution = 0.6
        worldAddBody(world, frontBumper)
    end

    local obstacles = {
        {x = 0, y = 0, r = 1.0}, {x = 3, y = 3, r = 0.6},
        {x = -3, y = 3, r = 0.6}, {x = 3, y = -3, r = 0.6},
        {x = -3, y = -3, r = 0.6},
    }
    for i = 1, #obstacles do
        local o = obstacles[i]
        local obs = createBody(createCircle(o.r), o.x, o.y, 1, true)
        obs.restitution = 0.7
        worldAddBody(world, obs)
    end

    return world
end

-- ============================================================================
-- Scenario 49: Assembly line (conveyor + sorting)
-- ============================================================================

function createAssemblyLineScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(30, 0.3), 0, -0.3, 1, true)
    worldAddBody(world, ground)

    local belts = {
        {x = -12, y = 2, w = 5, angle = 0, speed = 3},
        {x = -4, y = 2, w = 4, angle = -0.15, speed = 2},
        {x = 3, y = 1.5, w = 4, angle = 0, speed = 3.5},
        {x = 10, y = 1.5, w = 4, angle = 0.1, speed = 2.5},
    }

    for i = 1, #belts do
        local b = belts[i]
        local belt = createBody(createBox(b.w / 2, 0.15), b.x, b.y, 1, true)
        belt.angle = b.angle
        belt.dynamicFriction = 0.8
        worldAddBody(world, belt)

        local lipL = createBody(createBox(0.1, 0.2), b.x - b.w / 2 - 0.1, b.y + 0.2, 1, true)
        worldAddBody(world, lipL)
        local lipR = createBody(createBox(0.1, 0.2), b.x + b.w / 2 + 0.1, b.y + 0.2, 1, true)
        worldAddBody(world, lipR)
    end

    local sorterX = 6
    local sorterY = 4
    local sorterArm = createBody(createBox(1.5, 0.1), sorterX, sorterY, 2.0, false)
    worldAddBody(world, sorterArm)
    local sorterPivot = createBody(createCircle(0.1), sorterX, sorterY, 1, true)
    worldAddBody(world, sorterPivot)
    local sj = createRevoluteJoint(sorterPivot, sorterArm, vec(0, 0), vec(0, 0))
    sj.motorEnabled = true
    sj.motorSpeed = 2
    sj.maxMotorTorque = 20
    worldAddJoint(world, sj)

    resetRandom()
    for i = 1, 25 do
        local x = -15 + randomRange(-1, 1)
        local y = 4 + i * 0.8
        local choice = math_floor(random() * 4)
        local body
        if choice == 0 then
            body = createBody(createCircle(randomRange(0.2, 0.4)), x, y, 2.0, false)
        elseif choice == 1 then
            body = createBody(createBox(0.3, 0.3), x, y, 2.0, false)
        elseif choice == 2 then
            body = createBody(createRegularPolygon(0.3, 5), x, y, 2.0, false)
        else
            body = createBody(createRegularPolygon(0.25, 3), x, y, 2.0, false)
        end
        body.restitution = 0.2
        body.dynamicFriction = 0.3
        worldAddBody(world, body)
    end

    return world
end

-- ============================================================================
-- Scenario 50: Suspension bridge with traffic
-- ============================================================================

function createSuspensionBridgeScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(35, 0.5), 0, -0.5, 1, true)
    worldAddBody(world, ground)

    local bridgeLength = 24
    local bridgeY = 8
    local numDeckSegs = 20
    local segWidth = bridgeLength / numDeckSegs
    local startX = -bridgeLength / 2

    local leftTower = createBody(createBox(0.5, 5), startX - 1, bridgeY + 2.5, 1, true)
    worldAddBody(world, leftTower)
    local rightTower = createBody(createBox(0.5, 5), -startX + 1, bridgeY + 2.5, 1, true)
    worldAddBody(world, rightTower)

    local leftAnchor = createBody(createBox(0.3, 0.3), startX - 1, bridgeY + 5.5, 1, true)
    worldAddBody(world, leftAnchor)
    local rightAnchor = createBody(createBox(0.3, 0.3), -startX + 1, bridgeY + 5.5, 1, true)
    worldAddBody(world, rightAnchor)

    local deckSegs = {}
    local prevSeg = nil
    for i = 1, numDeckSegs do
        local x = startX + (i - 0.5) * segWidth
        local seg = createBody(createBox(segWidth / 2 - 0.02, 0.12), x, bridgeY, 3.0, false)
        seg.linearDamping = 0.1
        seg.angularDamping = 0.2
        worldAddBody(world, seg)
        deckSegs[i] = seg

        if prevSeg then
            local j = createRevoluteJoint(prevSeg, seg,
                vec(segWidth / 2 - 0.02, 0), vec(-segWidth / 2 + 0.02, 0))
            worldAddJoint(world, j)
        else
            local anchorJoint = createRevoluteJoint(leftTower, seg,
                vec(0.5, -2.5), vec(-segWidth / 2, 0))
            worldAddJoint(world, anchorJoint)
        end
        prevSeg = seg
    end
    local lastAnchorJoint = createRevoluteJoint(rightTower, deckSegs[numDeckSegs],
        vec(-0.5, -2.5), vec(segWidth / 2, 0))
    worldAddJoint(world, lastAnchorJoint)

    local numCables = 10
    for i = 1, numCables do
        local segIdx = math_floor(i * numDeckSegs / (numCables + 1))
        if segIdx < 1 then segIdx = 1 end
        if segIdx > numDeckSegs then segIdx = numDeckSegs end
        local seg = deckSegs[segIdx]
        local x = startX + (segIdx - 0.5) * segWidth
        local cableLen = 5 - math_abs(x) / bridgeLength * 3

        local anchorBody = (x < 0) and leftAnchor or rightAnchor
        local anchorLocalX = x - ((x < 0) and (startX - 1) or (-startX + 1))
        local cable = createDistanceJoint(anchorBody, seg,
            vec(anchorLocalX * 0.3, 0), vec(0, 0), cableLen)
        cable.stiffness = 150
        cable.damping = 5
        worldAddJoint(world, cable)
    end

    for i = 1, 4 do
        local x = startX + i * bridgeLength / 5
        local car = createBody(createBox(1.0, 0.4), x, bridgeY + 0.6, 5.0, false)
        car.velocity = vec(3, 0)
        car.dynamicFriction = 0.5
        worldAddBody(world, car)
    end

    return world
end

-- ============================================================================
-- Predefined obstacle courses (large data)
-- ============================================================================

local obstacleCourseData = {
    {type = "box", x = -12.5, y = 1.0, w = 0.5, h = 1.0, angle = 0, static = true},
    {type = "box", x = -11.0, y = 1.5, w = 0.5, h = 1.5, angle = 0, static = true},
    {type = "box", x = -9.5, y = 1.0, w = 1.0, h = 0.3, angle = -0.2, static = true},
    {type = "circle", x = -8.0, y = 2.0, r = 0.5, static = true},
    {type = "box", x = -6.5, y = 0.5, w = 0.3, h = 2.0, angle = 0, static = true},
    {type = "polygon", x = -5.0, y = 1.5, sides = 5, r = 0.7, static = true},
    {type = "box", x = -3.5, y = 2.0, w = 1.5, h = 0.2, angle = 0.3, static = true},
    {type = "circle", x = -2.0, y = 1.0, r = 0.4, static = true},
    {type = "box", x = -0.5, y = 2.5, w = 0.4, h = 0.4, angle = 0.785, static = true},
    {type = "box", x = 1.0, y = 1.0, w = 2.0, h = 0.2, angle = -0.15, static = true},
    {type = "circle", x = 3.0, y = 2.0, r = 0.6, static = true},
    {type = "polygon", x = 4.5, y = 1.5, sides = 6, r = 0.5, static = true},
    {type = "box", x = 6.0, y = 1.0, w = 0.5, h = 1.5, angle = 0.1, static = true},
    {type = "box", x = 7.5, y = 2.5, w = 1.0, h = 0.2, angle = -0.25, static = true},
    {type = "circle", x = 9.0, y = 1.5, r = 0.7, static = true},
    {type = "box", x = 10.5, y = 1.0, w = 0.3, h = 2.5, angle = 0, static = true},
    {type = "polygon", x = 12.0, y = 2.0, sides = 3, r = 0.8, static = true},
    {type = "box", x = -12.0, y = 4.0, w = 1.5, h = 0.2, angle = 0.2, static = true},
    {type = "circle", x = -10.0, y = 4.5, r = 0.5, static = true},
    {type = "box", x = -8.0, y = 3.5, w = 0.5, h = 1.0, angle = 0, static = true},
    {type = "polygon", x = -6.0, y = 4.0, sides = 4, r = 0.6, static = true},
    {type = "box", x = -4.0, y = 5.0, w = 2.0, h = 0.15, angle = -0.1, static = true},
    {type = "circle", x = -2.0, y = 4.0, r = 0.3, static = true},
    {type = "box", x = 0, y = 4.5, w = 0.8, h = 0.8, angle = 0.4, static = true},
    {type = "box", x = 2.0, y = 3.5, w = 1.0, h = 0.2, angle = 0.15, static = true},
    {type = "polygon", x = 4.0, y = 4.0, sides = 5, r = 0.4, static = true},
    {type = "circle", x = 6.0, y = 5.0, r = 0.8, static = true},
    {type = "box", x = 8.0, y = 4.0, w = 0.4, h = 1.5, angle = -0.2, static = true},
    {type = "box", x = 10.0, y = 4.5, w = 1.5, h = 0.2, angle = 0.3, static = true},
    {type = "circle", x = 12.0, y = 3.5, r = 0.5, static = true},
    {type = "box", x = -11.0, y = 7.0, w = 0.5, h = 0.5, angle = 0, static = true},
    {type = "box", x = -9.0, y = 6.5, w = 1.0, h = 0.2, angle = -0.3, static = true},
    {type = "circle", x = -7.0, y = 7.0, r = 0.6, static = true},
    {type = "polygon", x = -5.0, y = 6.0, sides = 6, r = 0.5, static = true},
    {type = "box", x = -3.0, y = 7.5, w = 1.5, h = 0.15, angle = 0.2, static = true},
    {type = "circle", x = -1.0, y = 6.5, r = 0.4, static = true},
    {type = "box", x = 1.0, y = 7.0, w = 0.6, h = 1.2, angle = 0, static = true},
    {type = "polygon", x = 3.0, y = 6.0, sides = 3, r = 0.7, static = true},
    {type = "box", x = 5.0, y = 7.0, w = 1.0, h = 0.2, angle = -0.15, static = true},
    {type = "circle", x = 7.0, y = 7.5, r = 0.5, static = true},
    {type = "box", x = 9.0, y = 6.5, w = 0.4, h = 1.8, angle = 0.1, static = true},
    {type = "polygon", x = 11.0, y = 7.0, sides = 5, r = 0.6, static = true},
}

function createObstacleCourseScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(15, 0.3), 0, -0.3, 1, true)
    worldAddBody(world, ground)

    for i = 1, #obstacleCourseData do
        local d = obstacleCourseData[i]
        local body
        if d.type == "box" then
            body = createBody(createBox(d.w, d.h), d.x, d.y, 1, d.static)
            if d.angle then body.angle = d.angle end
        elseif d.type == "circle" then
            body = createBody(createCircle(d.r), d.x, d.y, 1, d.static)
        elseif d.type == "polygon" then
            body = createBody(createRegularPolygon(d.r, d.sides), d.x, d.y, 1, d.static)
        end
        if body then
            body.restitution = 0.4
            worldAddBody(world, body)
        end
    end

    resetRandom()
    for i = 1, 15 do
        local x = randomRange(-13, -10)
        local y = randomRange(8, 14)
        local ball = createBody(createCircle(randomRange(0.2, 0.5)), x, y, 2.0, false)
        ball.restitution = 0.5
        ball.velocity = vec(randomRange(2, 6), randomRange(-2, 2))
        worldAddBody(world, ball)
    end

    return world
end

-- ============================================================================
-- Predefined building layout data (for city scenario)
-- ============================================================================

local buildingLayouts = {
    {x = -20, floors = 4, width = 3, style = "brick"},
    {x = -16, floors = 6, width = 2.5, style = "column"},
    {x = -12, floors = 3, width = 4, style = "brick"},
    {x = -7, floors = 8, width = 2, style = "column"},
    {x = -3, floors = 5, width = 3.5, style = "brick"},
    {x = 2, floors = 7, width = 2.5, style = "column"},
    {x = 6, floors = 4, width = 3, style = "brick"},
    {x = 10, floors = 6, width = 3, style = "column"},
    {x = 15, floors = 3, width = 4.5, style = "brick"},
    {x = 20, floors = 5, width = 2, style = "column"},
}

function createCityBlockScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local ground = createBody(createBox(30, 0.5), 0, -0.5, 1, true)
    ground.staticFriction = 0.8
    worldAddBody(world, ground)

    for bi = 1, #buildingLayouts do
        local bld = buildingLayouts[bi]
        local bx = bld.x
        local bw = bld.width
        local floorH = 1.0

        if bld.style == "brick" then
            local brickW = 0.5
            local brickH = 0.25
            local bricksPerRow = math_floor(bw / (brickW * 2)) + 1

            for floor = 0, bld.floors - 1 do
                local y = 0.25 + floor * (brickH * 2 + 0.01)
                local offset = (floor % 2 == 0) and 0 or brickW
                for col = 0, bricksPerRow - 1 do
                    local x = bx - bw / 2 + offset + col * brickW * 2
                    if x >= bx - bw / 2 and x <= bx + bw / 2 then
                        local brick = createBody(createBox(brickW * 0.9, brickH * 0.9), x, y, 2.5, false)
                        brick.restitution = 0.0
                        brick.staticFriction = 0.7
                        worldAddBody(world, brick)
                    end
                end
            end
        else
            local colW = 0.15
            local slabH = 0.08

            for floor = 0, bld.floors - 1 do
                local baseY = floor * floorH + 0.5

                local lc = createBody(createBox(colW, floorH / 2 - slabH),
                    bx - bw / 2 + colW, baseY + floorH / 2 - slabH, 3.0, false)
                lc.staticFriction = 0.6
                worldAddBody(world, lc)

                local rc = createBody(createBox(colW, floorH / 2 - slabH),
                    bx + bw / 2 - colW, baseY + floorH / 2 - slabH, 3.0, false)
                rc.staticFriction = 0.6
                worldAddBody(world, rc)

                local slab = createBody(createBox(bw / 2 + 0.1, slabH),
                    bx, baseY + floorH - slabH, 4.0, false)
                slab.staticFriction = 0.6
                worldAddBody(world, slab)
            end
        end
    end

    return world
end

-- ============================================================================
-- Terrain generation functions
-- ============================================================================

local function generateHillTerrain(startX, endX, segments, amplitude, frequency, baseY)
    local points = {}
    local segWidth = (endX - startX) / segments
    for i = 0, segments do
        local x = startX + i * segWidth
        local y = baseY + amplitude * math_sin(x * frequency) + amplitude * 0.5 * math_sin(x * frequency * 2.3 + 1.7)
        points[i + 1] = vec(x, y)
    end
    return points
end

local function generateStepTerrain(startX, endX, numSteps, stepHeight, baseY)
    local points = {}
    local stepWidth = (endX - startX) / numSteps
    for i = 0, numSteps do
        local x = startX + i * stepWidth
        local y = baseY + math_floor(i / 2) * stepHeight
        points[#points + 1] = vec(x, y)
        if i < numSteps then
            points[#points + 1] = vec(x + stepWidth, y)
        end
    end
    return points
end

local function buildTerrainBodies(world, points)
    for i = 1, #points - 1 do
        local p1 = points[i]
        local p2 = points[i + 1]
        local midX = (p1.x + p2.x) / 2
        local midY = (p1.y + p2.y) / 2
        local dx = p2.x - p1.x
        local dy = p2.y - p1.y
        local len = math_sqrt(dx * dx + dy * dy)
        if len > 0.01 then
            local seg = createBody(createBox(len / 2, 0.2), midX, midY, 1, true)
            seg.angle = math_atan2(dy, dx)
            seg.staticFriction = 0.8
            worldAddBody(world, seg)
        end
    end
end

-- ============================================================================
-- Scenario 51: Hill terrain with rolling objects
-- ============================================================================

function createHillTerrainScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local terrain = generateHillTerrain(-20, 20, 60, 2.0, 0.3, 0)
    buildTerrainBodies(world, terrain)

    resetRandom()
    for i = 1, 20 do
        local x = randomRange(-18, -10)
        local y = 5 + randomRange(0, 3)
        local choice = math_floor(random() * 3)
        local body
        if choice == 0 then
            body = createBody(createCircle(randomRange(0.3, 0.7)), x, y, 2.0, false)
        elseif choice == 1 then
            body = createBody(createBox(randomRange(0.3, 0.6), randomRange(0.3, 0.6)), x, y, 2.0, false)
        else
            body = createBody(createRegularPolygon(randomRange(0.3, 0.5), 5), x, y, 2.0, false)
        end
        body.restitution = 0.3
        body.dynamicFriction = 0.3
        worldAddBody(world, body)
    end

    return world
end

-- ============================================================================
-- Scenario 52: Step terrain with bouncing balls
-- ============================================================================

function createStepTerrainScenario()
    local world = createWorld(vec(0, -10), 3.0)

    local terrain = generateStepTerrain(-15, 15, 12, 0.8, 0)
    buildTerrainBodies(world, terrain)

    local wallL = createBody(createBox(0.3, 5), -16, 5, 1, true)
    worldAddBody(world, wallL)
    local wallR = createBody(createBox(0.3, 10), 16, 8, 1, true)
    worldAddBody(world, wallR)

    resetRandom()
    for i = 1, 30 do
        local x = randomRange(-14, 14)
        local y = randomRange(8, 15)
        local ball = createBody(createCircle(randomRange(0.2, 0.5)), x, y, 2.0, false)
        ball.restitution = randomRange(0.5, 0.9)
        ball.dynamicFriction = 0.2
        worldAddBody(world, ball)
    end

    return world
end

-- ============================================================================
-- Predefined joint configurations for mechanical tests
-- ============================================================================

local mechanismConfigs = {
    fourbar = {
        bodies = {
            {x = 0, y = 0, w = 0.1, h = 0.1, static = true},
            {x = 3, y = 0, w = 1.5, h = 0.1, static = false},
            {x = 6, y = 2, w = 1.2, h = 0.1, static = false},
            {x = 3, y = 4, w = 1.5, h = 0.1, static = false},
            {x = 0, y = 4, w = 0.1, h = 0.1, static = true},
        },
        joints = {
            {type = "revolute", a = 1, b = 2, ax = 0, ay = 0, bx = -1.5, by = 0},
            {type = "revolute", a = 2, b = 3, ax = 1.5, ay = 0, bx = -1.2, by = 0},
            {type = "revolute", a = 3, b = 4, ax = 1.2, ay = 0, bx = 1.5, by = 0},
            {type = "revolute", a = 4, b = 5, ax = -1.5, ay = 0, bx = 0, by = 0},
        }
    },
    crank_slider = {
        bodies = {
            {x = 0, y = 5, w = 0.1, h = 0.1, static = true},
            {x = 1.5, y = 5, w = 1.0, h = 0.08, static = false},
            {x = 4, y = 5, w = 1.5, h = 0.08, static = false},
            {x = 6, y = 5, w = 0.4, h = 0.3, static = false},
        },
        joints = {
            {type = "revolute", a = 1, b = 2, ax = 0, ay = 0, bx = -1.0, by = 0},
            {type = "revolute", a = 2, b = 3, ax = 1.0, ay = 0, bx = -1.5, by = 0},
            {type = "revolute", a = 3, b = 4, ax = 1.5, ay = 0, bx = 0, by = 0},
            {type = "prismatic", a = 1, b = 4, ax = 0, ay = 0, bx = 0, by = 0, axisX = 1, axisY = 0},
        }
    },
    scotch_yoke = {
        bodies = {
            {x = 0, y = 10, w = 0.1, h = 0.1, static = true},
            {x = 1, y = 10, w = 0.8, h = 0.08, static = false},
            {x = 3, y = 10, w = 1.0, h = 0.3, static = false},
        },
        joints = {
            {type = "revolute", a = 1, b = 2, ax = 0, ay = 0, bx = -0.8, by = 0},
            {type = "prismatic", a = 1, b = 3, ax = 0, ay = 0, bx = 0, by = 0, axisX = 1, axisY = 0},
            {type = "revolute", a = 2, b = 3, ax = 0.8, ay = 0, bx = 0, by = 0},
        }
    },
}

function createMechanismScenario()
    local world = createWorld(vec(0, 0), 3.0)
    world.gravity = vec(0, 0)

    for mechName, config in next, mechanismConfigs do
        local bodies = {}
        for i = 1, #config.bodies do
            local bd = config.bodies[i]
            local body = createBody(createBox(bd.w, bd.h), bd.x, bd.y, 2.0, bd.static)
            worldAddBody(world, body)
            bodies[i] = body
        end

        for i = 1, #config.joints do
            local jd = config.joints[i]
            local a = bodies[jd.a]
            local b = bodies[jd.b]
            if jd.type == "revolute" then
                local j = createRevoluteJoint(a, b, vec(jd.ax, jd.ay), vec(jd.bx, jd.by))
                if i == 1 then
                    j.motorEnabled = true
                    j.motorSpeed = 3
                    j.maxMotorTorque = 50
                end
                worldAddJoint(world, j)
            elseif jd.type == "prismatic" then
                local axis = vec(jd.axisX or 1, jd.axisY or 0)
                local j = createPrismaticJoint(a, b, vec(jd.ax, jd.ay), vec(jd.bx, jd.by), axis)
                worldAddJoint(world, j)
            end
        end
    end

    return world
end

-- ============================================================================
-- Energy and momentum analysis
-- ============================================================================

local function computeKineticEnergy(world)
    local ke = 0
    for i = 1, #world.bodies do
        local body = world.bodies[i]
        if not body.isStatic then
            local linKE = 0.5 * body.mass * vecLenSq(body.velocity)
            local angKE = 0.5 * body.inertia * body.angularVelocity * body.angularVelocity
            ke = ke + linKE + angKE
        end
    end
    return ke
end

local function computeMomentum(world)
    local px, py = 0, 0
    for i = 1, #world.bodies do
        local body = world.bodies[i]
        if not body.isStatic then
            px = px + body.mass * body.velocity.x
            py = py + body.mass * body.velocity.y
        end
    end
    return vec(px, py)
end

local function computeAngularMomentum(world, origin)
    origin = origin or vec(0, 0)
    local L = 0
    for i = 1, #world.bodies do
        local body = world.bodies[i]
        if not body.isStatic then
            local r = vecSub(body.position, origin)
            local p = vecMul(body.velocity, body.mass)
            L = L + vecCross(r, p)
            L = L + body.inertia * body.angularVelocity
        end
    end
    return L
end

local function computeCenterOfMass(world)
    local totalMass = 0
    local cx, cy = 0, 0
    for i = 1, #world.bodies do
        local body = world.bodies[i]
        if not body.isStatic then
            totalMass = totalMass + body.mass
            cx = cx + body.position.x * body.mass
            cy = cy + body.position.y * body.mass
        end
    end
    if totalMass > 0 then
        return vec(cx / totalMass, cy / totalMass), totalMass
    end
    return vec(0, 0), 0
end

-- ============================================================================
-- Scenario 53: Energy conservation test
-- ============================================================================

function createEnergyTestScenario()
    local world = createWorld(vec(0, 0), 4.0)
    world.gravity = vec(0, 0)

    local wallTop = createBody(createBox(10, 0.2), 0, 8, 1, true)
    wallTop.restitution = 1.0
    worldAddBody(world, wallTop)
    local wallBot = createBody(createBox(10, 0.2), 0, -8, 1, true)
    wallBot.restitution = 1.0
    worldAddBody(world, wallBot)
    local wallL = createBody(createBox(0.2, 8), -10, 0, 1, true)
    wallL.restitution = 1.0
    worldAddBody(world, wallL)
    local wallR = createBody(createBox(0.2, 8), 10, 0, 1, true)
    wallR.restitution = 1.0
    worldAddBody(world, wallR)

    resetRandom()
    for i = 1, 20 do
        local ball = createBody(createCircle(0.4), randomRange(-8, 8), randomRange(-6, 6), 2.0, false)
        ball.restitution = 1.0
        ball.dynamicFriction = 0.0
        ball.linearDamping = 0.0
        ball.velocity = vec(randomRange(-5, 5), randomRange(-5, 5))
        worldAddBody(world, ball)
    end

    return world
end

-- ============================================================================
-- Predefined simulation test cases with expected physics behavior
-- ============================================================================

local testCases = {
    {
        name = "free_fall",
        setup = function()
            local w = createWorld(vec(0, -10), 5.0)
            local ball = createBody(createCircle(0.5), 0, 10, 1.0, false)
            ball.linearDamping = 0
            worldAddBody(w, ball)
            return w
        end,
        steps = 10,
        check = function(world)
            local ball = world.bodies[1]
            return ball.position.y < 10 and ball.velocity.y < 0
        end
    },
    {
        name = "elastic_collision",
        setup = function()
            local w = createWorld(vec(0, 0), 5.0)
            w.gravity = vec(0, 0)
            local a = createBody(createCircle(0.5), -3, 0, 1.0, false)
            a.velocity = vec(5, 0)
            a.restitution = 1.0
            a.linearDamping = 0
            worldAddBody(w, a)
            local b = createBody(createCircle(0.5), 3, 0, 1.0, false)
            b.velocity = vec(-5, 0)
            b.restitution = 1.0
            b.linearDamping = 0
            worldAddBody(w, b)
            return w
        end,
        steps = 15,
        check = function(world)
            local a = world.bodies[1]
            local b = world.bodies[2]
            return a.velocity.x < 0 and b.velocity.x > 0
        end
    },
    {
        name = "stack_stability",
        setup = function()
            local w = createWorld(vec(0, -10), 3.0)
            w.iterations = 15
            local ground = createBody(createBox(5, 0.5), 0, -0.5, 1, true)
            ground.staticFriction = 0.9
            worldAddBody(w, ground)
            for i = 1, 5 do
                local box = createBody(createBox(0.4, 0.4), 0, i * 0.85, 2.0, false)
                box.staticFriction = 0.7
                box.restitution = 0.0
                worldAddBody(w, box)
            end
            return w
        end,
        steps = 30,
        check = function(world)
            for i = 2, #world.bodies do
                if world.bodies[i].position.x > 2 or world.bodies[i].position.x < -2 then
                    return false
                end
            end
            return true
        end
    },
    {
        name = "circle_on_slope",
        setup = function()
            local w = createWorld(vec(0, -10), 5.0)
            local slope = createBody(createBox(5, 0.2), 0, 3, 1, true)
            slope.angle = -0.3
            slope.staticFriction = 0.2
            worldAddBody(w, slope)
            local ball = createBody(createCircle(0.3), -3, 5, 2.0, false)
            ball.dynamicFriction = 0.1
            worldAddBody(w, ball)
            return w
        end,
        steps = 20,
        check = function(world)
            return world.bodies[2].velocity.x > 0
        end
    },
    {
        name = "pendulum_swing",
        setup = function()
            local w = createWorld(vec(0, -10), 3.0)
            local anchor = createBody(createCircle(0.1), 0, 10, 1, true)
            worldAddBody(w, anchor)
            local bob = createBody(createCircle(0.3), 3, 10, 3.0, false)
            worldAddBody(w, bob)
            local j = createDistanceJoint(anchor, bob, vec(0, 0), vec(0, 0), 3)
            j.stiffness = 500
            j.damping = 0.5
            worldAddJoint(w, j)
            return w
        end,
        steps = 30,
        check = function(world)
            return math_abs(world.bodies[2].position.x) < 3.5
        end
    },
}

function runTestCases()
    local allPassed = true
    for i = 1, #testCases do
        local tc = testCases[i]
        bodyIdCounter = 0
        local world = tc.setup()
        for step = 1, tc.steps do
            worldStep(world, 1/60)
        end
        if not tc.check(world) then
            allPassed = false
        end
    end
    return allPassed
end

-- ============================================================================
-- Additional predefined body configurations
-- ============================================================================

local predefWorlds = {}

predefWorlds.tower_of_circles = function()
    local world = createWorld(vec(0, -10), 2.0)
    local ground = createBody(createBox(10, 0.3), 0, -0.3, 1, true)
    worldAddBody(world, ground)
    for i = 1, 30 do
        local radius = 0.4 - i * 0.005
        if radius < 0.15 then radius = 0.15 end
        local ball = createBody(createCircle(radius), 0, i * radius * 2 + 0.5, 2.0, false)
        ball.restitution = 0.0
        ball.staticFriction = 0.8
        worldAddBody(world, ball)
    end
    return world
end

predefWorlds.falling_grid = function()
    local world = createWorld(vec(0, -10), 2.0)
    local ground = createBody(createBox(12, 0.3), 0, -0.3, 1, true)
    worldAddBody(world, ground)
    local cols = 8
    local rows = 8
    local spacing = 1.0
    for r = 0, rows - 1 do
        for c = 0, cols - 1 do
            local x = (c - cols / 2) * spacing + 0.5
            local y = 5 + r * spacing
            local body = createBody(createBox(0.35, 0.35), x, y, 2.0, false)
            body.restitution = 0.1
            worldAddBody(world, body)
        end
    end
    return world
end

predefWorlds.spinning_shapes = function()
    local world = createWorld(vec(0, -10), 3.0)
    local ground = createBody(createBox(15, 0.3), 0, -0.3, 1, true)
    worldAddBody(world, ground)
    resetRandom()
    for i = 1, 20 do
        local x = randomRange(-10, 10)
        local y = randomRange(5, 15)
        local sides = math_floor(random() * 5) + 3
        local body = createBody(createRegularPolygon(randomRange(0.3, 0.8), sides), x, y, 2.0, false)
        body.angularVelocity = randomRange(-10, 10)
        body.restitution = 0.4
        worldAddBody(world, body)
    end
    return world
end

predefWorlds.heavy_on_light = function()
    local world = createWorld(vec(0, -10), 3.0)
    local ground = createBody(createBox(8, 0.3), 0, -0.3, 1, true)
    worldAddBody(world, ground)
    for i = 1, 8 do
        local density = 0.5 + (8 - i) * 2
        local body = createBody(createBox(2 - i * 0.15, 0.3), 0, i * 0.65, density, false)
        body.restitution = 0.0
        body.staticFriction = 0.7
        worldAddBody(world, body)
    end
    return world
end

predefWorlds.chain_curtain = function()
    local world = createWorld(vec(0, -10), 2.0)
    local numChains = 10
    local linksPerChain = 8
    local chainSpacing = 1.5
    local startX = -(numChains - 1) * chainSpacing / 2

    for c = 0, numChains - 1 do
        local x = startX + c * chainSpacing
        local anchor = createBody(createCircle(0.1), x, 12, 1, true)
        worldAddBody(world, anchor)
        local prev = anchor
        for l = 1, linksPerChain do
            local link = createBody(createBox(0.2, 0.1), x, 12 - l * 0.5, 1.5, false)
            link.angularDamping = 0.3
            worldAddBody(world, link)
            local j = createDistanceJoint(prev, link, vec(0, -0.1), vec(0, 0.1), 0.3)
            j.stiffness = 200
            j.damping = 5
            worldAddJoint(world, j)
            prev = link
        end
    end
    return world
end

predefWorlds.avalanche = function()
    local world = createWorld(vec(0, -10), 2.0)
    local slopeAngle = -0.4
    local slope = createBody(createBox(15, 0.3), 0, 5, 1, true)
    slope.angle = slopeAngle
    slope.staticFriction = 0.3
    worldAddBody(world, slope)
    local ground = createBody(createBox(20, 0.3), 5, -2, 1, true)
    worldAddBody(world, ground)
    resetRandom()
    for i = 1, 40 do
        local x = randomRange(-12, -2)
        local y = 6 + randomRange(0, 4)
        local r = randomRange(0.15, 0.4)
        local ball = createBody(createCircle(r), x, y, 2.0, false)
        ball.restitution = 0.2
        ball.dynamicFriction = 0.3
        worldAddBody(world, ball)
    end
    return world
end

predefWorlds.trampoline = function()
    local world = createWorld(vec(0, -10), 3.0)
    local frame_l = createBody(createBox(0.2, 1), -4, 1, 1, true)
    worldAddBody(world, frame_l)
    local frame_r = createBody(createBox(0.2, 1), 4, 1, 1, true)
    worldAddBody(world, frame_r)
    local numSegs = 12
    local segWidth = 8 / numSegs
    local prev = frame_l
    for i = 1, numSegs do
        local x = -4 + (i - 0.5) * segWidth
        local seg = createBody(createBox(segWidth / 2 - 0.02, 0.05), x, 1.5, 0.5, false)
        worldAddBody(world, seg)
        local j = createDistanceJoint(prev, seg, vec(0.2, 0), vec(-segWidth / 2, 0), 0.05)
        j.stiffness = 300
        j.damping = 5
        worldAddJoint(world, j)
        prev = seg
    end
    local lastJ = createDistanceJoint(prev, frame_r, vec(segWidth / 2, 0), vec(-0.2, 0), 0.05)
    lastJ.stiffness = 300
    lastJ.damping = 5
    worldAddJoint(world, lastJ)
    local ball = createBody(createCircle(0.5), 0, 8, 5.0, false)
    ball.restitution = 0.8
    worldAddBody(world, ball)
    return world
end

predefWorlds.domino_spiral = function()
    local world = createWorld(vec(0, -10), 3.0)
    local ground = createBody(createBox(15, 0.3), 0, -0.3, 1, true)
    worldAddBody(world, ground)
    local numDominoes = 30
    local spiralRadius = 5
    for i = 0, numDominoes - 1 do
        local angle = i * 0.25
        local r = spiralRadius - i * 0.1
        if r < 1 then r = 1 end
        local x = r * math_cos(angle)
        local y = 0.7
        local domino = createBody(createBox(0.1, 0.6), x, y, 3.0, false)
        domino.angle = angle + math_pi / 2
        domino.staticFriction = 0.5
        worldAddBody(world, domino)
    end
    local pusher = createBody(createCircle(0.3), spiralRadius + 0.5, 1, 8.0, false)
    pusher.velocity = vec(-5, 0)
    worldAddBody(world, pusher)
    return world
end

-- ============================================================================
-- Run simulation and checksum
-- ============================================================================

local function checksumWorld(world)
    local sum = 0
    for i = 1, #world.bodies do
        local body = world.bodies[i]
        sum = sum + body.position.x * 1000
        sum = sum + body.position.y * 1000
        sum = sum + body.velocity.x * 100
        sum = sum + body.velocity.y * 100
        sum = sum + body.angle * 500
        sum = sum + body.angularVelocity * 50
    end
    return math_floor(sum * 1000) / 1000
end

local function runScenario(createFn, steps, name)
    bodyIdCounter = 0
    local world = createFn()
    for step = 1, steps do
        worldStep(world, 1 / 60)
    end
    return checksumWorld(world)
end

local function runScenarioExtended(createFn, steps, name)
    bodyIdCounter = 0
    local world = createFn()
    for step = 1, steps do
        worldStepExtended(world, 1 / 60)
    end
    return checksumWorld(world)
end

function runScenariosGroup1()
    local result = 0
    result = result + runScenario(createBoxStackScenario, 8, "BoxStack")
    result = result + runScenario(createPendulumScenario, 6, "Pendulum")
    result = result + runScenario(createBallPitScenario, 6, "BallPit")
    result = result + runScenario(createDominoScenario, 10, "Domino")
    result = result + runScenario(createBilliardsScenario, 5, "Billiards")
    result = result + runScenario(createTumblerScenario, 5, "Tumbler")
    result = result + runScenario(createBridgeScenario, 6, "Bridge")
    result = result + runScenario(createCradleScenario, 5, "Cradle")
    result = result + runScenarioExtended(createVehicleScenario, 8, "Vehicle")
    result = result + runScenarioExtended(createWreckingBallScenario, 6, "WreckingBall")
    result = result + runScenarioExtended(createGearTrainScenario, 5, "GearTrain")
    result = result + runScenarioExtended(createClothScenario, 5, "Cloth")
    result = result + runScenarioExtended(createConveyorScenario, 5, "Conveyor")
    result = result + runScenarioExtended(createCatapultScenario, 8, "Catapult")
    result = result + runScenarioExtended(createPinballScenario, 6, "Pinball")
    result = result + runScenarioExtended(createRubeGoldbergScenario, 8, "RubeGoldberg")
    result = result + runScenarioExtended(createGranularScenario, 5, "Granular")
    result = result + runScenarioExtended(createRagdollScenario, 6, "Ragdoll")
    result = result + runScenarioExtended(createBreakableChainScenario, 5, "BreakableChain")
    result = result + runScenarioExtended(createMixedStackScenario, 5, "MixedStack")
    return result
end

function runScenariosGroup2()
    bodyIdCounter = 0
    local _, rayCount, aabbCount, pointCount = createRaycastTestScenario()
    local result = rayCount * 1000 + aabbCount * 100 + pointCount

    result = result + createParticleRopeScenario()
    result = result + createParticleClothScenario()
    result = result + createSoftBodyScenario()

    bodyIdCounter = 0
    local world = createBuoyancyScenario()
    for step = 1, 10 do
        for fi = 1, #world.floaters do
            applyBuoyancy(world.floaters[fi], world.waterLevel, world.waterDensity, world.dragCoeff)
        end
        worldStep(world, 1/60)
    end
    result = result + checksumWorld(world)

    bodyIdCounter = 0
    world = createTornadoScenario()
    for step = 1, 12 do
        for di = 1, #world.debris do
            local body = world.debris[di]
            if not body.isStatic then
                local toCenter = vecSub(world.vortexCenter, body.position)
                local dist = vecLen(toCenter)
                if dist > 0.5 then
                    local tangent = vecPerp(vecNormalize(toCenter))
                    local tangentialForce = vecMul(tangent, world.vortexStrength * body.mass / dist)
                    local radialForce = vecMul(toCenter, 5 * body.mass / (dist * dist))
                    bodyApplyForce(body, vecAdd(tangentialForce, radialForce))
                end
            end
        end
        worldStep(world, 1/60)
    end
    result = result + checksumWorld(world)

    result = result + runScenario(createLargePyramidScenario, 4, "LargePyramid")
    result = result + runScenario(createMarbleRunScenario, 6, "MarbleRun")
    result = result + runScenario(createExplosionScenario, 5, "Explosion")
    result = result + runScenarioExtended(createPulleyScenario, 5, "Pulley")
    result = result + runScenario(createElasticChainScenario, 5, "ElasticChain")
    result = result + runScenario(createMaterialTestScenario, 5, "MaterialTest")
    result = result + runScenario(createComplexPolygonScenario, 6, "ComplexPolygon")
    result = result + runScenario(createStressTestScenario, 4, "StressTest")
    result = result + runScenario(createCastleScenario, 4, "Castle")
    result = result + runScenarioExtended(createClockworkScenario, 5, "Clockwork")
    result = result + runScenarioExtended(createTrebuchetScenario, 6, "Trebuchet")
    result = result + runScenario(createFluidScenario, 4, "Fluid")
    result = result + runScenarioExtended(createWindmillScenario, 5, "Windmill")
    result = result + runScenarioExtended(createDetailedVehicleScenario, 6, "DetailedVehicle")
    return result
end

function runScenariosGroup3()
    local result = 0
    result = result + runScenario(createBowlingScenario, 8, "Bowling")
    result = result + runScenario(createEarthquakeScenario, 4, "Earthquake")
    result = result + runScenario(createPachinkoScenario, 5, "Pachinko")
    result = result + runScenarioExtended(createSpringLatticeScenario, 4, "SpringLattice")
    result = result + runScenario(createCannonScenario, 6, "Cannon")
    result = result + runScenario(createWreckingYardScenario, 4, "WreckingYard")
    result = result + runScenario(createRaceTrackScenario, 5, "RaceTrack")
    result = result + runScenario(createRollerCoasterScenario, 5, "RollerCoaster")
    result = result + runScenario(createDestructionDerbyScenario, 5, "DestructionDerby")
    result = result + runScenarioExtended(createAssemblyLineScenario, 5, "AssemblyLine")
    result = result + runScenarioExtended(createSuspensionBridgeScenario, 5, "SuspensionBridge")
    result = result + runScenario(createObstacleCourseScenario, 5, "ObstacleCourse")
    result = result + runScenario(createCityBlockScenario, 4, "CityBlock")
    result = result + runScenario(createHillTerrainScenario, 5, "HillTerrain")
    result = result + runScenario(createStepTerrainScenario, 5, "StepTerrain")
    result = result + runScenarioExtended(createMechanismScenario, 5, "Mechanism")
    result = result + runScenario(createEnergyTestScenario, 5, "EnergyTest")
    result = result + runScenario(predefWorlds.tower_of_circles, 5, "TowerCircles")
    result = result + runScenario(predefWorlds.falling_grid, 4, "FallingGrid")
    result = result + runScenario(predefWorlds.spinning_shapes, 5, "SpinningShapes")
    result = result + runScenario(predefWorlds.heavy_on_light, 5, "HeavyOnLight")
    result = result + runScenarioExtended(predefWorlds.chain_curtain, 4, "ChainCurtain")
    result = result + runScenario(predefWorlds.avalanche, 5, "Avalanche")
    result = result + runScenarioExtended(predefWorlds.trampoline, 5, "Trampoline")
    result = result + runScenario(predefWorlds.domino_spiral, 5, "DominoSpiral")

    local tcResult = runTestCases()
    result = result + (tcResult and 1 or 0)

    return result
end

function runAllScenarios()
    local result = 0
    result = result + runScenariosGroup1()
    result = result + runScenariosGroup2()
    result = result + runScenariosGroup3()
    return result
end

-- First run to establish expected values
local result = runAllScenarios()
if result ~= 21502896.173 then
    error("Bad checksum " .. result)
end

end

bench.runCode(test, "physics")
