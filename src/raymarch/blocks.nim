import vmath
const
  MaxLightStrength = 16
  ChunkEdgeSize* = 50i32
  ChunkSize* = ChunkEdgeSize * ChunkEdgeSize * ChunkEdgeSize

type
  Block* {.size: sizeof(int16).} = enum
    air
    dirt
    stone
    tree
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
