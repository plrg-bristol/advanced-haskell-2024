data Gender = Male
            | Female
            | NonBinary
            deriving Eq

data Position = Keeper
             | Chaser
             | Seeker
             | Beater
             deriving Eq

--deep
data QuidditchTeam = End
                   | Player Position Gender QuidditchTeam

teamSize :: QuidditchTeam -> Int
teamSize End = 0
teamSize (Player p g qt) = 1 + teamSize qt

noGender :: Gender -> QuidditchTeam -> Int
noGender g End = 0
noGender g (Player p g' qt)
                | g' == g   = 1 + noGender g qt
                | otherwise =  noGender g qt

noPosition :: Position -> QuidditchTeam -> Int
noPosition p End = 0
noPosition p (Player p' g qt)
                | p' == p   = 1 + noPosition p qt
                | otherwise =  noPosition p qt


example :: String -> QuidditchTeam
example "Good"      = Player Beater Male (Player Beater Male (Player Seeker NonBinary (Player Chaser Female (Player Chaser Female (Player Chaser Female (Player Keeper NonBinary (End)))))))
example "tooBig"    = Player Beater Male (Player Beater Male (Player Seeker NonBinary (Player Chaser Female (Player Chaser Female (Player Chaser Female (Player Keeper NonBinary (Player Keeper NonBinary End)))))))
example "badGender" = Player Beater Male (Player Beater Male (Player Seeker Male (Player Chaser Male (Player Chaser Male (Player Chaser Male (Player Keeper Male End))))))
example "wrongPos"  = Player Beater Female (Player Beater Female (Player Beater Female (Player Chaser Male (Player Chaser Male (Player Chaser Male (Player Keeper Male End))))))

type SnitchON = Bool

validTeam :: SnitchON -> QuidditchTeam -> Bool
validTeam True t = (teamSize t <= 7) 
                && (noPosition Seeker t <= 1)
                && (noPosition Beater t <= 2)
                && (noPosition Chaser t <= 3)
                && (noPosition Keeper t <= 1)
                && (noGender Male t <= 4)
                && (noGender Female t <= 4)
                && (noGender NonBinary t <= 4)
validTeam False t = (teamSize t <= 6) 
                && (noPosition Seeker t <= 0)
                && (noPosition Beater t <= 2)
                && (noPosition Chaser t <= 3)
                && (noPosition Keeper t <= 1)
                && (noGender Male t <= 4)
                && (noGender Female t <= 4)
                && (noGender NonBinary t <= 4)

-- shallow

type Male      = Int
type Female    = Int
type NonBinary = Int
type Keeper    = Int
type Chaser    = Int
type Seeker    = Int
type Beater    = Int
type TeamSize  = Int

end :: Bool -> (Male, Female, NonBinary, Keeper, Chaser, Seeker, Beater, TeamSize, SnitchON, Bool)
end snitch = (0, 0, 0, 0, 0, 0, 0, 0, snitch, True)

player :: Position -> Gender -> (Male, Female, NonBinary, Keeper, Chaser, Seeker, Beater, TeamSize, SnitchON, Bool) -> (Male, Female, NonBinary, Keeper, Chaser, Seeker, Beater, TeamSize, SnitchON, Bool)
player Keeper Male      (m, f, nb, k, c, s, b, t, snitch, g)  =  ((m+1), f, nb, (k+1), c, s, b, (t+1), snitch, (goodTeam (m, f, nb, k, c, s, b, t, snitch, g)))
player Keeper Female    (m, f, nb, k, c, s, b, t, snitch, g)  =  (m, (f+1), nb, (k+1), c, s, b, (t+1), snitch, (goodTeam (m, f, nb, k, c, s, b, t, snitch, g)))
player Keeper NonBinary (m, f, nb, k, c, s, b, t, snitch, g)  =  (m, f, (nb+1), (k+1), c, s, b, (t+1), snitch, (goodTeam (m, f, nb, k, c, s, b, t, snitch, g)))
player Seeker Male      (m, f, nb, k, c, s, b, t, snitch, g)  =  ((m+1), f, nb, k, c, (s+1), b, (t+1), snitch, (goodTeam (m, f, nb, k, c, s, b, t, snitch, g)))
player Seeker Female    (m, f, nb, k, c, s, b, t, snitch, g)  =  (m, (f+1), nb, k, c, (s+1), b, (t+1), snitch, (goodTeam (m, f, nb, k, c, s, b, t, snitch, g)))
player Seeker NonBinary (m, f, nb, k, c, s, b, t, snitch, g)  =  (m, f, (nb+1), k, c, (s+1), b, (t+1), snitch, (goodTeam (m, f, nb, k, c, s, b, t, snitch, g)))
player Chaser Male      (m, f, nb, k, c, s, b, t, snitch, g)  =  ((m+1), f, nb, k, (c+1), s, b, (t+1), snitch, (goodTeam (m, f, nb, k, c, s, b, t, snitch, g)))
player Chaser Female    (m, f, nb, k, c, s, b, t, snitch, g)  =  (m, (f+1), nb, k, (c+1), s, b, (t+1), snitch, (goodTeam (m, f, nb, k, c, s, b, t, snitch, g)))
player Chaser NonBinary (m, f, nb, k, c, s, b, t, snitch, g)  =  (m, f, (nb+1), k, (c+1), s, b, (t+1), snitch, (goodTeam (m, f, nb, k, c, s, b, t, snitch, g)))
player Beater Male      (m, f, nb, k, c, s, b, t, snitch, g)  =  ((m+1), f, nb, k, c, s, (b+1), (t+1), snitch, (goodTeam (m, f, nb, k, c, s, b, t, snitch, g)))
player Beater Female    (m, f, nb, k, c, s, b, t, snitch, g)  =  (m, (f+1), nb, k, c, s, (b+1), (t+1), snitch, (goodTeam (m, f, nb, k, c, s, b, t, snitch, g)))
player Beater NonBinary (m, f, nb, k, c, s, b, t, snitch, g)  =  (m, f, (nb+1), k, c, s, (b+1), (t+1), snitch, (goodTeam (m, f, nb, k, c, s, b, t, snitch, g)))


goodTeam :: (Male, Female, NonBinary, Keeper, Chaser, Seeker, Beater, TeamSize, SnitchON, Bool) -> Bool
goodTeam (m, f, nb, k, c, s, b, t,True, g) = ( m <= 4) 
                                          && ( f <= 4)
                                          && ( nb<= 4)
                                          && ( k <= 1)
                                          && ( c <= 3)
                                          && ( s <= 1)
                                          && ( b <= 2)
                                          && ( t <= 7)
                                          && g
goodTeam (m, f, nb, k, c, s, b, t,False, g) = ( m <= 4) 
                                          && ( f <= 4)
                                          && ( nb<= 4)
                                          && ( k <= 1)
                                          && ( c <= 3)
                                          && ( s <= 0)
                                          && ( b <= 2)
                                          && ( t <= 6)
                                          && g


example1 :: String -> (Male, Female, NonBinary, Keeper, Chaser, Seeker, Beater, TeamSize, SnitchON, Bool)
example1 "Good"       =  player Beater Male (player Beater Male (player Seeker NonBinary (player Chaser Female (player Chaser Female (player Chaser Female (player Keeper NonBinary (end True)))))))
example1 "tooBig"     =  player Beater Male (player Beater Male (player Seeker NonBinary (player Chaser Female (player Chaser Female (player Chaser Female (player Keeper NonBinary (player Keeper NonBinary (end True))))))))
example1 "badGender"  =  player Beater Male (player Beater Male (player Seeker Male (player Chaser Male (player Chaser Male (player Chaser Male (player Keeper Male (end True)))))))
example1 "wrongPos"   =  player Beater Female (player Beater Female (player Beater Female (player Chaser Male (player Chaser Male (player Chaser Male (player Keeper Male (end True)))))))
