module Enemy (moveEnemyTowards) where

moveEnemyTowards :: (Float, Float) -> (Float, Float) -> (Float, Float)
moveEnemyTowards (px, py) (ex, ey) =
    let dx = px - ex
        dy = py - ey
        distance = sqrt (dx^2 + dy^2)
        speed = 0.5  -- Enemy speed
        stepX = if distance > 0 then dx / distance * speed else 0
        stepY = if distance > 0 then dy / distance * speed else 0
    in (ex + stepX, ey + stepY)
