-- Mesh generation from heightmap (vertices and normals)

export type Mesh = {
    vertices: {number},
    normals: {number},
    vertexCount: number,
}

local function generateMesh(heightmap: {number}, size: number, scale: number): Mesh
    local vertexCount = (size - 1) * (size - 1) * 6
    local vertices = table.create(vertexCount * 3, 0)
    local normals = table.create(vertexCount * 3, 0)
    local vIdx = 1
    local nIdx = 1

    for y = 0, size - 2 do
        for x = 0, size - 2 do
            local h00 = heightmap[y * size + x + 1]
            local h10 = heightmap[y * size + x + 2]
            local h01 = heightmap[(y + 1) * size + x + 1]
            local h11 = heightmap[(y + 1) * size + x + 2]

            local x0 = x * scale
            local x1 = (x + 1) * scale
            local z0 = y * scale
            local z1 = (y + 1) * scale

            -- Triangle 1: (x0,h00,z0), (x1,h10,z0), (x0,h01,z1)
            vertices[vIdx] = x0; vertices[vIdx+1] = h00 * scale; vertices[vIdx+2] = z0
            vertices[vIdx+3] = x1; vertices[vIdx+4] = h10 * scale; vertices[vIdx+5] = z0
            vertices[vIdx+6] = x0; vertices[vIdx+7] = h01 * scale; vertices[vIdx+8] = z1

            local ux = x1 - x0; local uy = (h10 - h00) * scale; local uz = 0
            local vx = 0; local vy = (h01 - h00) * scale; local vz = z1 - z0
            local nx = uy * vz - uz * vy
            local ny = uz * vx - ux * vz
            local nz = ux * vy - uy * vx
            local nLen = math.sqrt(nx*nx + ny*ny + nz*nz)
            if nLen > 0 then nx /= nLen; ny /= nLen; nz /= nLen end
            normals[nIdx] = nx; normals[nIdx+1] = ny; normals[nIdx+2] = nz
            normals[nIdx+3] = nx; normals[nIdx+4] = ny; normals[nIdx+5] = nz
            normals[nIdx+6] = nx; normals[nIdx+7] = ny; normals[nIdx+8] = nz
            vIdx += 9
            nIdx += 9

            -- Triangle 2: (x1,h10,z0), (x1,h11,z1), (x0,h01,z1)
            vertices[vIdx] = x1; vertices[vIdx+1] = h10 * scale; vertices[vIdx+2] = z0
            vertices[vIdx+3] = x1; vertices[vIdx+4] = h11 * scale; vertices[vIdx+5] = z1
            vertices[vIdx+6] = x0; vertices[vIdx+7] = h01 * scale; vertices[vIdx+8] = z1

            ux = 0; uy = (h11 - h10) * scale; uz = z1 - z0
            vx = x0 - x1; vy = (h01 - h10) * scale; vz = z1 - z0
            nx = uy * vz - uz * vy
            ny = uz * vx - ux * vz
            nz = ux * vy - uy * vx
            nLen = math.sqrt(nx*nx + ny*ny + nz*nz)
            if nLen > 0 then nx /= nLen; ny /= nLen; nz /= nLen end
            normals[nIdx] = nx; normals[nIdx+1] = ny; normals[nIdx+2] = nz
            normals[nIdx+3] = nx; normals[nIdx+4] = ny; normals[nIdx+5] = nz
            normals[nIdx+6] = nx; normals[nIdx+7] = ny; normals[nIdx+8] = nz
            vIdx += 9
            nIdx += 9
        end
    end

    return {
        vertices = vertices,
        normals = normals,
        vertexCount = vertexCount,
    }
end

local function computeStats(heightmap: {number}, size: number): (number, number, number)
    local minH = heightmap[1]
    local maxH = heightmap[1]
    local sumH = 0
    local n = size * size
    for i = 1, n do
        local h = heightmap[i]
        if h < minH then minH = h end
        if h > maxH then maxH = h end
        sumH += h
    end
    return minH, maxH, sumH / n
end

return {
    generateMesh = generateMesh,
    computeStats = computeStats,
}
