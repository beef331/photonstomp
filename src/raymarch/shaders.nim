import opengl
import std/[typetraits, tables]
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
out vec4 fragColor;


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

layout (std430, binding = 3) buffer blockData
{
  int ids[];
};

uniform float time;
uniform float epsilon;

uniform int chunkSize;

struct RayResult{
  int hit;
  vec3 normal;
  vec3 pos;
};

uniform float MinStep = 0.01;

RayResult rayMarch(vec2 coord){
  RayResult result;
  vec3 ray = normalize(mat3(camera.matrix) * vec3(coord, 1));

  ivec3 pos = ivec3(camera.pos_dist.xyz);

  ivec3 step = ivec3(sign(ray));

  ivec3 nextVoxel = ivec3(pos + step);

  vec3 tmax = vec3( ray.x >= 0 ? (nextVoxel.x - pos.x) / ray.x: MinStep,
                    ray.y >= 0 ? (nextVoxel.y - pos.y) / ray.y: MinStep,
                    ray.z >= 0 ? (nextVoxel.z - pos.z) / ray.z: MinStep);

  vec3 delta = vec3(1) / ray * step;
  delta.x = ray.x != 0 ? delta.x: MinStep;
  delta.y = ray.y != 0 ? delta.y: MinStep;
  delta.z = ray.z != 0 ? delta.z: MinStep;

  float total = 0;
  for(int i = 0; i < camera.pos_dist.w; i++){
    if(tmax.x < tmax.y && tmax.x < tmax.z){
      total = tmax.x;
      result.normal = vec3(float(-step.x), 0.0, 0.0);

      pos.x += step.x;
      tmax.x += delta.x;
    }
    else if(tmax.y < tmax.z){
      total = tmax.y;
      result.normal = vec3(0.0, float(-step.y), 0.0);

      pos.y += step.y;
      tmax.y += delta.y;
    }else{
      total = tmax.z;
      result.normal = vec3(0.0, 0.0, float(-step.z));

      pos.z += step.z;
      tmax.z += delta.z;
    }
    if(pos.x >= 0 && pos.x < chunkSize &&
       pos.y >= 0 && pos.y < chunkSize &&
       pos.z >= 0 && pos.z < chunkSize){
      int index = pos.x + (pos.z * chunkSize) + (pos.y * chunkSize * chunkSize);
      int blockId = (ids[index / 2] >> (index % 2 * 0x10) & 0xffff); //int32 -> int16

      if(blockId > 0){
        result.hit = blockId;
        result.pos = camera.pos_dist.xyz + (total + 0.01) * ray;
        return result;
      }
    }
  }
  return result;
} 

void main()
{
  vec2 uv = vec2(gl_FragCoord.xy - 0.5 * camera.size.xy) / camera.size.y;
  vec3 lightPos = light.pos.xyz;

  RayResult res = rayMarch(uv);
  if(res.hit > 0){
    vec3 col = vec3(1);
    //fragColor = vec4(col, 1);
    float light = dot(res.normal, normalize(lightPos)) * 0.5 + 0.5;
    fragColor = vec4(col * light, 1);
    //fragColor = vec4((res.normal * 0.5 + 0.5), 1);
    //fragColor = vec4(res.pos / vec3(chunkSize), 1);
  }
  else{
    fragColor = vec4(0);
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


const UboTable = {
  "Camera": 1.Gluint,
  "Light": 2.Gluint
  }.toTable

proc genUbo*[T; U: static[string]](shader: Gluint): Ubo[T] =
  glGenBuffers(1, result.Gluint.addr)
  glBindBuffer(GlUniformBuffer, result.Gluint)
  glBindBufferbase(GlUniformBuffer, UboTable[U], result.Gluint) # Apparently no way to go name -> Ubo bind location

proc copyTo*[T](val: T, ubo: Ubo[T]) =
  glBindBuffer(GlUniformBuffer, ubo.GLuint)
  glNamedBufferData(ubo.Gluint, sizeof(T), val.unsafeAddr, GlDynamicDraw)
  glBindBuffer(GlUniformBuffer, 0.Gluint)

proc genSsbo*[T](shader: Gluint, binding: Gluint): Ssbo[T] =
  glCreateBuffers(1, result.Gluint.addr)
  glBindBufferbase(GlShaderStorageBuffer, binding, result.Gluint)

proc copyTo*[T](val: T, ssbo: Ssbo[T]) =
  glBindBuffer(GlShaderStorageBuffer, ssbo.GLuint)
  glBufferData(GlShaderStorageBuffer, sizeof(T).GLsizeiptr, val.unsafeAddr, GlDynamicDraw)
  glBindBuffer(GlShaderStorageBuffer, 0.Gluint)


proc setUniform*(shader: Gluint, uniform: string, value: float32) =
  let loc = glGetUniformLocation(shader, uniform)
  if loc != -1:
    glUniform1f(loc, value.GlFloat)

proc setUniform*(shader: Gluint, uniform: string, value: int32) =
  let loc = glGetUniformLocation(shader, uniform)
  if loc != -1:
    glUniform1i(loc, value.Glint)
