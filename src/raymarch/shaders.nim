import opengl
import std/[options, macros]
const
  vertexShader = """
#version 330 core
layout (location = 0) in vec3 aPos;
  
void main()
{
    gl_Position = vec4(aPos, 1.0); // see how we directly give a vec3 to vec4's constructor
}
"""
  fragShader = """
#version 330 core
out vec4 FragColor;
  
in vec4 vertexColor; // the input variable from the vertex shader (same name and same type)

uniform Camera
{
  vec3 pos;
  vec2 size;
  float distance;
} camera;

uniform float time;

struct RayResult{
  float hit;
  vec3 normal;
  vec3 rayDir;
};

RayResult rayMarch(vec2 coord){
  RayResult result;
  result.rayDir = normalize(vec3(coord - vec2(0.5), 1));
  vec3 pos = vec3(sin(time) * 3.0, 2, -10);
  for(int i = 0; i < 1000; i++){
    vec3 normal = normalize(pos);
    if(length(pos) <= 2){
      result.normal = normal;
      result.hit = 1;
      break;
    }
    pos += abs(distance(pos, normal)) * result.rayDir;
  }
  return result;
} 


void main()
{
  vec2 uv = gl_FragCoord.xy / camera.size;
  uv.y *= camera.size.y / camera.size.x;
  RayResult res = rayMarch(uv);
  if(res.hit > 0){
    vec3 color = (res.normal + 1) / 2;
    FragColor = vec4(color, 1);
  }else{
    FragColor = vec4(0);
  }
} 
"""
type
  ShaderKind* = enum
    Vertex, Fragment, Compute

const KindLut = [
  Vertex: GlVertexShader,
  Fragment: GlFragmentShader,
  Compute: GlComputeShader
]


proc loadShader*(shader: string, kind: ShaderKind): Option[Gluint] =
  let
    shaderProg = allocCStringArray([shader])
    shaderId = glCreateShader(KindLut[kind])
  glShaderSource(shaderId, 1, shaderProg, nil)
  glCompileShader(shaderId)
  var success = 0.Glint
  glGetShaderiv(shaderId, GlCompileStatus, success.addr)

  if success == 0:
    var buff = newString(512)
    glGetShaderInfoLog(shaderId, 512, nil, buff[0].addr)
    echo buff
    return
  result = some(shaderId)

  shaderProg.deallocCStringArray

proc getDefaultShader*(): Gluint =
  let
    vs = loadShader(vertexShader, Vertex)
    fs = loadShader(fragShader, Fragment)
  if vs.isSome and fs.isSome:
    result = glCreateProgram()
    glAttachShader(result, vs.get)
    glAttachShader(result, fs.get)
    glLinkProgram(result)

    var success = 1.Glint

    glGetProgramIv(result, GlLinkStatus, success.addr)
    if success == 0:
      var msg = newString(512)
      glGetProgramInfoLog(result, 512, nil, msg[0].addr)
      echo msg
    glDeleteShader(vs.get)
    glDeleteShader(fs.get)

proc setUniformBuff*[T: object](shader: Gluint, uniform: string, value: T) =
  let
    blockIndex = glGetUniformBlockIndex(shader, uniform)
    blockSize = 0.Glint

  shader.glGetActiveUniformBlockiv(blockIndex, GlUniformBlockDataSize, blockSize.unsafeAddr)
  echo sizeOf(value), " ", blockSize
  assert sizeof(value) <= blockSize

  var uboHandle: GLuint
  glGenBuffers(1, uboHandle.addr)
  glBindBuffer(GlUniformBuffer, uboHandle)
  glBufferData(GlUniformBuffer, blockSize, value.unsafeAddr, GlDynamicDraw)
  glBindBufferBase(GlUniformBuffer, blockIndex, uboHandle)

proc setUniform*(shader: Gluint, uniform: string, value: float32) =
  let loc = glGetUniformLocation(shader, uniform)
  if loc != -1:
    glUniform1f(loc, value.GlFloat)

macro alignToShader*(typeDef: untyped): untyped =
  result = typeDef
  for identDef in result[2][2]:
    for ind in 0 .. identDef.len - 3:
      let field = identDef[ind]
      identDef[ind] = quote do:
        `field`{.align: 16.}
