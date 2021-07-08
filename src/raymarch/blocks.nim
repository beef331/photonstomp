import vmath, noisy
import std/[cpuinfo, strformat]
import cameras

const
  MaxLightStrength = 16
  ChunkEdgeSize* = 300
  ChunkEdgeSqr* = ChunkEdgeSize * ChunkEdgeSize
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

  BlockModificationKind* {.pure.} = enum
    single, circle, box

  BlockModification* = object
    pos*: int
    newBlock*: Block
    case kind*: BlockModificationKind
    of box:
      width*, height*, depth*: int
    of circle:
      radius*: int
    else: discard

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
  
  ThreadData = object
    id: int
    data: ptr Chunk

func genChunk*(seed: int, scale: float): Chunk =
  var noise = initSimplex(seed)
  for i in 0 ..< ChunkEdgeSize * ChunkEdgeSize:
    let
      x = i mod ChunkEdgeSize
      z = i mod (ChunkEdgeSize * ChunkEdgeSize) div ChunkEdgeSize
      height = (noise.value(x.float * scale , z.float * scale) + 1) / 2 * 80

    for x in 0 .. height.int:
      let ind = i + x * ChunkEdgeSize * ChunkEdgeSize
      if x <= 20:
        result[ind] = water
      elif x <= 30:
        result[ind] = sand
      elif x <= 40:
        result[ind] = stone
      elif x <= 50:
        result[ind] = dirt
      else:
        result[ind] = grass

template x(i: int): int = i mod ChunkEdgeSize
template y(i: int): int = i div ChunkEdgeSqr
template z(i: int): int = i mod ChunkEdgeSqr div ChunkEdgeSize

proc toIndex(v: IVec3): int = v.x + v.y * ChunkEdgeSqr + v.z * ChunkEdgeSize

iterator modifyBlocks*(data: ptr Chunk, modif: BlockModification): Slice[int] =
  case modif.kind
  of single:
    data[modif.pos] = modif.newBlock
    yield Slice[int](a: modif.pos, b: modif.pos)
  of box:
    let
      w = min(ChunkEdgeSize - (modif.pos.x + modif.width), modif.width)
      h = min(ChunkEdgeSize - (modif.pos.y + modif.height), modif.height)
      d = min(ChunkEdgeSize - (modif.pos.z + modif.depth), modif.depth)
    for y in 0..h:
      let pos = modif.pos + y * ChunkEdgeSqr
      for x in 0..w:
        for z in 0..d:
          data[pos + x + z * ChunkEdgeSize] = modif.newBlock
      yield Slice[int](a: pos, b: pos + d * ChunkEdgeSize)
  of circle:
    let
      xMin =
        if modif.pos.x - modif.radius < 0:
          modif.pos.x
        else:
          modif.radius
      yMin =
        if modif.pos.y - modif.radius < 0:
          modif.pos.y
        else:
          modif.radius
      zMin =
        if modif.pos.z - modif.radius < 0:
          modif.pos.z
        else:
          modif.radius
      xMax =
        if modif.pos.x + modif.radius < ChunkEdgeSize:
          modif.radius * 2
        else:
          ChunkEdgeSize - modif.pos.x
      yMax =
        if modif.pos.y + modif.radius < ChunkEdgeSize:
          modif.radius * 2
        else:
          ChunkEdgeSize - modif.pos.y
      zMax = 
        if modif.pos.z + modif.radius < ChunkEdgeSize:
          modif.radius * 2
        else:
          ChunkEdgeSize - modif.pos.z
      pos = modif.pos - xMin - (zMin * ChunkEdgeSize) - (yMin * ChunkEdgeSqr)
      spherePos = ivec3(modif.pos.x.int32, modif.pos.y.int32, modif.pos.z.int32)
    for y in 0..yMax:
      let pos = pos + y * ChunkEdgeSqr
      for x in 0..xMax:
        for z in 0..zMax:
          let blkPos = ivec3((pos.x + x).int32,(pos.y + y).int32, (pos.z + z).int32)
          if blkPos.distSq(spherePos) <= modif.radius * modif.radius:
            data[pos + x + z * ChunkEdgeSize] = modif.newBlock
      yield Slice[int](a: pos, b: pos + xMax + zMax * ChunkEdgeSize)

var
  modifs: seq[BlockModification]
  toDispatch = 0
  queueChannel* = newSeq[Channel[BlockModification]](countProcessors() - 1)
  sliceChannel*: Channel[Slice[int]]
  modifThreads = newSeq[Thread[ThreadData]](countProcessors() - 1)


proc blockThread(threadData: ThreadData){.thread.} =
  while true:
    {.cast(gcSafe).}:
      let modif = queueChannel[threadData.id].recv
      for x in threadData.data.modifyBlocks(modif):
        sliceChannel.send(x)

proc initBlockThreads*(data: ptr Chunk) =
  sliceChannel.open()
  var i = 0
  for x in modifThreads.mitems:
    queueChannel[i].open
    x.createThread(blocKThread, ThreadData(id: i, data: data))
    inc i

proc queueModification*(modif: BlockModification) = 
  #modifs.add(modif) # todo - use this to dispatch cleanly
  queueChannel[toDispatch].send(modif)
  toDispatch = (toDispatch + 1 + modifThreads.len) mod modifThreads.len

template sign[T](vec: GVec3[T]): Gvec3[T] = gvec3(T(vec.x.sign), T(vec.y.sign), T(vec.z.sign))

proc raycast*(camera: Camera, data: Chunk): int =
  result = -1
  let
    ray = camera.matrix * vec3(0, 0, 1)
    deltaDist = abs(length(ray)) / ray
    rayStep = ivec3(ray.x.sign.int32, ray.y.sign.int32, ray.z.sign.int32)
  var
    pos = ivec3(camera.pos + camera.matrix * vec3(0.5, 0, 0) + camera.matrix * vec3(0, 0.5, 0))
    sideDist = (sign(ray) * (vec3(pos) - camera.pos) + (sign(ray) * 0.5) + 0.5) * deltaDist
  
  for i in 0..ChunkEdgeSize:
    if pos.x in 0..ChunkEdgeSize and 
       pos.y in 0..ChunkEdgeSize and
       pos.z in 0..ChunkEdgeSize and data[pos.toIndex] != air: return pos.toIndex
    if sideDist.x < sideDist.y and sideDist.x < sideDist.z:
      sideDist.x += deltaDist.x
      pos.x += rayStep.x
    elif sideDist.y < sideDist.z:
      sideDist.y += deltaDist.y
      pos.y += rayStep.y
    else:
      sideDist.z += deltaDist.z
      pos.z += rayStep.z