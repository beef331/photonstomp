import vmath
const
  MaxLightStrength = 16
  ChunkEdgeSize* = 128
  ChunkSize = ChunkEdgeSize * ChunkEdgeSize * ChunkEdgeSize

type
  Block* = uint16
  LightBlock* = object
    r* {.bitSize(4).}: 0u8..16u8
    g* {.bitSize(4).}: 0u8..16u8
    b* {.bitSize(4).}: 0u8..16u8
    global* {.bitSize(4).}: 0u8..16u8
  LightEmitter* = object
    pos: Vec3
    colour: LightBlock
    strength: 0..MaxLightStrength
  Chunk* = array[ChunkSize, Block]
  LightData* = array[ChunkSize, LightBlock]
