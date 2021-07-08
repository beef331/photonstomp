import sdl2/sdl, opengl, vmath, pixie
import raymarch/[shaders, cameras, blocks, textures]
import std/[monotimes, times, random]

const
  WindowFlags = WindowOpenGl

type App = object
  window: Window
  context: GLContext
  isRunning: bool
  rect: Gluint

var camera = Camera(size: vec4(1280, 720, 0, 0), distance: 1000f)
camera.pos = vec3(ChunkEdgeSize.float + 3, 200,  -3)
camera.matrix = rotateY((45).toRadians) * rotateX((-45).toRadians)

proc init: App =
  if init(INIT_VIDEO) == 0:
    result.isRunning = true
    discard glSetAttribute(GL_CONTEXT_MAJOR_VERSION, 4)
    discard glSetAttribute(GL_CONTEXT_MINOR_VERSION, 3)

    result.window = createWindow("Test", WindowPosUndefined, WindowPosUndefined, camera.size.x.int,
        camera.size.y.int, WindowFlags)
    result.context = glCreateContext(result.window)
    loadExtensions()
    glClearColor(0.0, 0.0, 0.0, 1)
    discard glSetSwapInterval(0.cint)

var
  dt = 0f
  scale = 0.05
  dirtied = false
  pressedDestroy = false
  pressedSpace = false

proc poll(app: var App) =
  var e: Event
  while pollEvent(addr(e)) != 0:
    # Quit requested
    if e.kind == Quit:
      app.isRunning = false
    # Key pressed
    elif e.kind == KeyDown:
      case e.key.keysym.sym:
      of K_ESCAPE:
        app.isRunning = false
      of K_w:
        camera.pos += camera.matrix * vec3(0, 0, 1) * dt * 300
      of K_s:
        camera.pos += camera.matrix * vec3(0, 0, -1) * dt * 300
      of K_q:
        camera.pos.y -= dt * 300
      of K_e:
        camera.pos.y += dt * 300
      of K_a:
        camera.pos += camera.matrix * vec3(-1, 0, 0) * dt * 300
      of K_d:
        camera.pos += camera.matrix * vec3(1, 0, 0) * dt * 300
      of K_r:
        scale += dt * 0.1
        dirtied = true
      of K_f:
        scale -= dt * 0.1
        dirtied = true
      of K_h:
        pressedDestroy = true
      of K_space:
        pressedSpace = true

      else: discard

proc draw(app: var App) =
  glClear(GL_ColorBufferBit)
  glDrawArrays(GL_TRIANGLES, 0, 3)
  glDrawArrays(GL_TRIANGLES, 2, 3)
  glSwapWindow(app.window)

proc bindRect(shader: Gluint) =
  let verts = [
    vec2(-1, -1),
    vec2(-1, 1),
    vec2(1, 1),
    vec2(1, -1),
    vec2(-1, -1)]
  let vbo: GLuint = 0
  glGenBuffers(1, vbo.unsafeaddr)
  glBindBuffer(GlArrayBuffer, vbo)
  glBufferData(GlArrayBuffer, verts.sizeof, verts.unsafeaddr, GlStaticDraw)

  let vao: Gluint = 0
  glGenVertexArrays(1, vao.unsafeAddr)
  glBindVertexArray(vao)

  let posAttrib = glGetAttribLocation(shader, "aPos").Gluint
  glVertexAttribPointer(posAttrib, 2.GlInt, cGlFloat, GlFalse, 0.GlInt, nil)
  glEnableVertexAttribArray(posAttrib);

proc openGlDebug(source: GLenum,
    typ: GLenum,
    id: GLuint,
    severity: GLenum,
    length: GLsizei,
    message: ptr GLchar,
    userParam: pointer) {.stdcall.} =
  var str = newString(length)
  copyMem(str[0].addr, message, length)
  echo str


var app = init()
let shader = getDefaultShader()

bindRect(shader)
glUseProgram(shader)
glEnable(GlDebugOutput)
glDebugMessageCallback(openGlDebug, nil)
var
  time = 0f
  lightPos = vec3(-1, -1, 0)
  chunkData = genChunk(321, scale)

initBlockThreads(chunkData.addr)

let
  cameraUbo = shader.genUbo[:Camera, "Camera"]()
  lightUbo = shader.genUbo[:Vec3, "Light"]()
  blockSsbo = shader.genSsbo[:Chunk](3)
  texId = genTexture()
  tileMap = readImage("./assets/tilesheet.png")

tileMap.copyTo texId
camera.copyTo cameraUbo
lightPos.copyTo lightUbo
chunkData.copyTo blockSsbo

shader.setUniform("chunkSize", ChunkEdgeSize)
shader.setUniform("tilemap", texId)
shader.setUniform("tileinfo", vec4(1f / tilemap.width.float, 20f, 8f, 0f))

while app.isRunning:
  let startTime = getMonoTime()
  app.poll
  app.draw
  dt = (getMonoTime() - startTime).inMicroseconds.float / 1_000_000f
  time += dt
  camera.copyTo cameraUbo
  shader.setUniform("time", time)

  if dirtied:
    chunkData = genChunk(321, scale)
    dirtied = false
    chunkData.copyTo blockSsbo

  if pressedDestroy:
    for x in 0..4:
      let
        x = rand(0..<ChunkEdgeSize)
        y = rand(0..<ChunkEdgeSize)
        z = rand(0..<ChunkEdgeSize)
        pos = x + y * ChunkEdgeSqr + z * ChunkEdgeSize
      queueModification(BlockModification(
        kind: BlockModificationKind.circle,
        pos: pos,
        radius: rand(10..20)))
    pressedDestroy = false
    queueModification(BlockModification(
      kind: BlockModificationKind.box,
      width: ChunkEdgeSize,
      depth: ChunkEdgeSize,
      height: 3))

  if pressedSpace:
    let pos = camera.raycast(chunkData)
    if pos > 0:
      queueModification(BlockModification(
        kind: BlockModificationKind.circle,
        pos: pos,
        radius: rand(10)))
    pressedSpace = false
  while true:
    let res = sliceChannel.tryRecv()
    if not res[0]: break
    chunkData.copyTo blockSsbo, res[1]

  echo 1 / dt

glDeleteContext(app.context)
app.window.destroyWindow

