import sdl2/sdl, opengl, vmath, pixie
import raymarch/[shaders, cameras, blocks, textures]
import std/[monotimes, times]

const
  WindowFlags = WindowOpenGl

type App = object
  window: Window
  context: GLContext
  isRunning: bool
  rect: Gluint

var camera = Camera(size: vec4(1280, 720, 0, 0), distance: 100f)
camera.pos = vec3(3, 0, -3)
camera.matrix = rotateY((45).toRadians)

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

var dt = 0f

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
        camera.pos += camera.matrix * vec3(0, 0, 1) * dt * 10
      of K_s:
        camera.pos += camera.matrix * vec3(0, 0, -1) * dt * 10
      of K_q:
        camera.pos.y -= dt * 10
      of K_e:
        camera.pos.y += dt * 10
      of K_a:
        camera.pos += camera.matrix * vec3(-1, 0, 0) * dt * 10
      of K_d:
        camera.pos += camera.matrix * vec3(1, 0, 0) * dt * 10

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
  lightPos = vec3(0, 1, 1)
  chunkData: Chunk

var i = 0
for blk in chunkData.mitems:
  let
    x = i mod ChunkEdgeSize
    y = i div (ChunkEdgeSize * ChunkEdgeSize)
    z = i mod (ChunkEdgeSize * ChunkEdgeSize) div ChunkEdgeSize
  if y <= 2:
    blk = dirt
  if x < 4:
    blk = stone
  if y == 9:
    blk = stone
  inc i

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

var
  lastStep = 3f
  lastId = 0i32

while app.isRunning:
  let startTime = getMonoTime()
  app.poll
  app.draw
  dt = (getMonoTime() - startTime).inMicroseconds.float / 1_000_000f
  time += dt

  if time - lastStep >= 0.01:
    chunkData[lastId] = air
    lastId = (lastId + chunkData.len + 1) mod chunkData.len
    chunkData[lastId] = stone
    #chunkData.copyTo blockSsbo
    lastStep = time

  #lightPos.x = sin(time) * 10
  #lightPos.copyTo lightUbo
  camera.copyTo cameraUbo
  shader.setUniform("time", time)

glDeleteContext(app.context)
app.window.destroyWindow

