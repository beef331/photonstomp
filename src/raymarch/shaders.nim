import opengl, vmath
import cameras
import std/typetraits
const
  vertexShader = """
#version 430
layout (location = 0) in vec3 aPos;
  
void main()
{
    gl_Position = vec4(aPos, 1.0); // see how we directly give a vec3 to vec4's constructor
}
"""
  fragShader = """
#version 430
out vec4 FragColor;


layout (std140, binding = 1) uniform Camera
{
  vec4 pos_dist;
  vec4 size;
  mat4 matrix;
} camera;

layout (std140, binding = 2) uniform Light
{
  vec3 pos;
} light;

layout (std140, binding = 3) buffer blockData
{
  int ids[];
};

uniform float time;
uniform int chunkSize;

struct RayResult{
  int hit;
  vec3 normal;
  vec3 rayDir;
};
/*
RayResult rayMarchSphere(vec2 coord){
  RayResult result;
  result.rayDir = (camera.matrix * vec4(normalize(vec3(coord - vec2(0.5), 1)), 1)).xyz;
  vec3 pos = camera.pos_dist.xyz;
  for(int i = 0; i < camera.pos_dist.w; i++){
    vec3 normal = normalize(pos);
    float offset = abs(distance(pos, normal * chunkSize));
    if(length(pos + offset * result.rayDir) <= chunkSize){
      result.normal = normal;
      result.hit = 1;
      break;
    }
    pos += offset * result.rayDir;
  }
  return result;
}
*/

RayResult rayMarch(vec2 coord){
  RayResult result;
  result.rayDir = normalize((camera.matrix * vec4(coord - vec2(0.5), 1, 1)).xyz);
  vec3 pos = camera.pos_dist.xyz;
  for(int i = 0; i < camera.pos_dist.w; i++){
    vec3 floored = floor(pos);
    if(floored.x >= 0 && floored.x <= chunkSize &&
      floored.y >= 0 && floored.y <= chunkSize &&
      floored.z >= 0 && floored.z <= chunkSize){

      int index = int(floored.x + floored.z * chunkSize + floored.y * chunkSize * chunkSize);
      int blockId = int(ids[index / 2] >> int(mod(index, 2) * 16));
      if(blockId > 0){
        result.hit = blockId;
        break;
      }
    }
    float offset = length(vec3(1) - fract(pos));
    pos += normalize(result.rayDir) * offset;
  }
  return result;
} 

void main()
{
  vec2 uv = gl_FragCoord.xy / camera.size.xy;
  uv.y *= camera.size.y / camera.size.x;
  vec3 lightPos = light.pos.xyz;
  RayResult res = rayMarch(uv);
  if(res.hit > 0){
    FragColor = vec4(1);
  }else{
    FragColor = vec4(0);
  }
} 
"""
type
  ShaderKind* = enum
    Vertex, Fragment, Compute
  Ubo*[T] = distinct Gluint
  Ssbo*[T] = distinct Gluint
  Shader* = distinct Gluint

const KindLut = [
  Vertex: GlVertexShader,
  Fragment: GlFragmentShader,
  Compute: GlComputeShader
]


proc loadShader*(shader: string, kind: ShaderKind): Gluint =
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
  result = shaderId

  shaderProg.deallocCStringArray

proc getDefaultShader*(): GLuint =
  let
    vs = loadShader(vertexShader, Vertex)
    fs = loadShader(fragShader, Fragment)
  result = glCreateProgram()
  glAttachShader(result, vs)
  glAttachShader(result, fs)
  glLinkProgram(result)

  var success = 1.Glint

  glGetProgramIv(result, GlLinkStatus, success.addr)
  if success == 0:
    var msg = newString(512)
    glGetProgramInfoLog(result, 512, nil, msg[0].addr)
    echo msg
  glDeleteShader(vs)
  glDeleteShader(fs)


proc genUbo*[T](shader: Gluint, uniform: string): Ubo[T] =
  let index = glGetUniformBlockIndex(shader, uniform)
  glGenBuffers(1, result.Gluint.addr)
  glBindBuffer(GlUniformBuffer, result.Gluint)
  glBindBufferbase(GlUniformBuffer, index, result.Gluint)

proc copyToBuffer*[T](ubo: Ubo[T], val: T) =
  glBindBuffer(GlUniformBuffer, ubo.GLuint)
  glNamedBufferData(ubo.Gluint, sizeof(T), val.unsafeAddr, GlDynamicDraw)
  glBindBuffer(GlUniformBuffer, 0.Gluint)

proc copyToBuffer*[T](ssbo: Ssbo[T], val: T) =
  glBindBuffer(GlShaderStorageBuffer, ssbo.GLuint)
  glBufferData(GlShaderStorageBuffer, sizeof(T).GLsizeiptr, val.unsafeAddr, GlDynamicDraw)
  glBindBuffer(GlShaderStorageBuffer, 0.Gluint)

proc genSsbo*[T](shader: Gluint, binding: Gluint): Ssbo[T] =
  glCreateBuffers(1, result.Gluint.addr)
  glBindBufferbase(GlShaderStorageBuffer, binding, result.Gluint)

proc setUniform*(shader: Gluint, uniform: string, value: float32) =
  let loc = glGetUniformLocation(shader, uniform)
  if loc != -1:
    glUniform1f(loc, value.GlFloat)

proc setUniform*(shader: Gluint, uniform: string, value: int32) =
  let loc = glGetUniformLocation(shader, uniform)
  if loc != -1:
    glUniform1i(loc, value.Glint)
