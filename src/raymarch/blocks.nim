import vmath, noisy
const
  MaxLightStrength = 16
  ChunkEdgeSize* = 1000
  ChunkSize* = ChunkEdgeSize * ChunkEdgeSize * ChunkEdgeSize

type
  Block* {.size: sizeof(int16).} = enum
    air
    dirt
    stone
    grass
    sand
    water
  Chunk* = array[ChunkSize, Block]

  LightBlock* = object
    r* {.bitSize(4).}: 0u8..16u8
    g* {.bitSize(4).}: 0u8..16u8
    b* {.bitSize(4).}: 0u8..16u8
    global* {.bitSize(4).}: 0u8..16u8
  LightEmitter* = object
    pos: Vec3
    colour: LightBlock
    strength: 0..MaxLightStrength

  LightData* = array[ChunkSize, LightBlock]

func genChunk*(seed: int): Chunk =
  var noise = initSimplex(seed)
  let grid = noise.grid((0, 0), (ChunkEdgeSize, ChunkEdgeSize))
  for i in 0 ..< ChunkEdgeSize * ChunkEdgeSize:
    let
      x = i mod ChunkEdgeSize
      z = i mod (ChunkEdgeSize * ChunkEdgeSize) div ChunkEdgeSize
      height = abs((grid[x, z] + 1 / 2)) * 10
    for x in 0 .. height.int:
      let ind = i + x * ChunkEdgeSize * ChunkEdgeSize
      if x <= 2:
        result[ind] = water
      elif x <= 5:
        result[ind] = sand
      elif x <= 7:
        result[ind] = stone
      elif x <= 8:
        result[ind] = dirt
      else:
        result[ind] = grass