-- Hydraulic and thermal erosion simulation

local function hydraulicErosion(
    heightmap: {number},
    size: number,
    iterations: number,
    seed: number
): {number}
    local map = table.create(size * size, 0)
    table.move(heightmap, 1, size * size, 1, map)

    local inertia = 0.05
    local capacity = 4.0
    local deposition = 0.3
    local erosionRate = 0.3
    local evaporation = 0.01
    local gravity = 4.0
    local minSlope = 0.01
    local maxDropletLife = 30

    for iter = 1, iterations do
        seed = bit32.band(seed * 1103515245 + 12345, 0x7FFFFFFF)
        local px = (seed % (size - 2)) + 1
        seed = bit32.band(seed * 1103515245 + 12345, 0x7FFFFFFF)
        local py = (seed % (size - 2)) + 1

        local posX = px + 0.5
        local posY = py + 0.5
        local dirX = 0.0
        local dirY = 0.0
        local speed = 1.0
        local water = 1.0
        local sediment = 0.0

        for life = 1, maxDropletLife do
            local cellX = math.floor(posX)
            local cellY = math.floor(posY)

            if cellX < 1 or cellX >= size - 1 or cellY < 1 or cellY >= size - 1 then
                break
            end

            local offsetX = posX - cellX
            local offsetY = posY - cellY

            local idx00 = cellY * size + cellX + 1
            local idx10 = cellY * size + cellX + 2
            local idx01 = (cellY + 1) * size + cellX + 1
            local idx11 = (cellY + 1) * size + cellX + 2

            local h00 = map[idx00]
            local h10 = map[idx10]
            local h01 = map[idx01]
            local h11 = map[idx11]

            local gradX = (h10 - h00) * (1 - offsetY) + (h11 - h01) * offsetY
            local gradY = (h01 - h00) * (1 - offsetX) + (h11 - h10) * offsetX

            dirX = dirX * inertia - gradX * (1 - inertia)
            dirY = dirY * inertia - gradY * (1 - inertia)

            local dirLen = math.sqrt(dirX * dirX + dirY * dirY)
            if dirLen < 0.0001 then
                seed = bit32.band(seed * 1103515245 + 12345, 0x7FFFFFFF)
                local angle = (seed % 628) / 100.0
                dirX = math.cos(angle)
                dirY = math.sin(angle)
                dirLen = 1
            end
            dirX /= dirLen
            dirY /= dirLen

            local newPosX = posX + dirX
            local newPosY = posY + dirY

            local newCellX = math.floor(newPosX)
            local newCellY = math.floor(newPosY)
            if newCellX < 1 or newCellX >= size - 1 or newCellY < 1 or newCellY >= size - 1 then
                break
            end

            local newOffX = newPosX - newCellX
            local newOffY = newPosY - newCellY
            local nIdx00 = newCellY * size + newCellX + 1
            local nIdx10 = newCellY * size + newCellX + 2
            local nIdx01 = (newCellY + 1) * size + newCellX + 1
            local nIdx11 = (newCellY + 1) * size + newCellX + 2

            local newHeight = map[nIdx00] * (1 - newOffX) * (1 - newOffY)
                + map[nIdx10] * newOffX * (1 - newOffY)
                + map[nIdx01] * (1 - newOffX) * newOffY
                + map[nIdx11] * newOffX * newOffY

            local oldHeight = h00 * (1 - offsetX) * (1 - offsetY)
                + h10 * offsetX * (1 - offsetY)
                + h01 * (1 - offsetX) * offsetY
                + h11 * offsetX * offsetY

            local heightDiff = newHeight - oldHeight

            local sedimentCapacity = math.max(-heightDiff, minSlope) * speed * water * capacity
            if sedimentCapacity < 0 then sedimentCapacity = 0 end

            if sediment > sedimentCapacity or heightDiff > 0 then
                local amount
                if heightDiff > 0 then
                    amount = math.min(heightDiff, sediment)
                else
                    amount = (sediment - sedimentCapacity) * deposition
                end
                sediment -= amount

                map[idx00] += amount * (1 - offsetX) * (1 - offsetY)
                map[idx10] += amount * offsetX * (1 - offsetY)
                map[idx01] += amount * (1 - offsetX) * offsetY
                map[idx11] += amount * offsetX * offsetY
            else
                local amount = math.min((sedimentCapacity - sediment) * erosionRate, -heightDiff)
                sediment += amount

                map[idx00] -= amount * (1 - offsetX) * (1 - offsetY)
                map[idx10] -= amount * offsetX * (1 - offsetY)
                map[idx01] -= amount * (1 - offsetX) * offsetY
                map[idx11] -= amount * offsetX * offsetY
            end

            speed = math.sqrt(math.max(0, speed * speed + heightDiff * gravity))
            water *= (1 - evaporation)
            posX = newPosX
            posY = newPosY
        end
    end

    return map
end

local function thermalErosion(
    heightmap: {number},
    size: number,
    iterations: number,
    talusAngle: number
): {number}
    local map = table.create(size * size, 0)
    table.move(heightmap, 1, size * size, 1, map)

    local dt = 0.5
    local neighbors = {{-1,0}, {1,0}, {0,-1}, {0,1}, {-1,-1}, {1,-1}, {-1,1}, {1,1}}
    local dists = {1, 1, 1, 1, 1.414, 1.414, 1.414, 1.414}

    for iter = 1, iterations do
        for y = 1, size - 2 do
            for x = 1, size - 2 do
                local idx = y * size + x + 1
                local h = map[idx]
                local maxDiff = 0
                local totalDiff = 0
                local diffs = table.create(8, 0)

                for n = 1, 8 do
                    local nx = x + neighbors[n][1]
                    local ny = y + neighbors[n][2]
                    local nIdx = ny * size + nx + 1
                    local diff = h - map[nIdx]
                    local threshold = talusAngle * dists[n]
                    if diff > threshold then
                        local excess = diff - threshold
                        diffs[n] = excess
                        totalDiff += excess
                        if diff > maxDiff then maxDiff = diff end
                    end
                end

                if totalDiff > 0 then
                    local moveAmount = dt * maxDiff * 0.5
                    for n = 1, 8 do
                        if diffs[n] > 0 then
                            local share = diffs[n] / totalDiff * moveAmount
                            local nx = x + neighbors[n][1]
                            local ny = y + neighbors[n][2]
                            local nIdx = ny * size + nx + 1
                            map[idx] -= share
                            map[nIdx] += share
                        end
                    end
                end
            end
        end
    end

    return map
end

return {
    hydraulicErosion = hydraulicErosion,
    thermalErosion = thermalErosion,
}
