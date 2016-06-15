--Solutions to DailyProgrammer problems
dp_217e d h
  | h > d = (1/d) * (dp_217e d (h - d))
  | otherwise = (d-h+1) / d
