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

uniform sampler2D tilemap;
uniform vec4 tileinfo; // x = texelSize, y = tile count in axis, z = tile size

struct RayResult{
  int hit;
  vec3 normal;
  vec2 uv;
  vec3 pos;
  float hitDist;
};

uniform float MinStep = 0.01;

bool isValid(ivec3 pos){
  return pos.x >= 0 && pos.x < chunkSize &&
         pos.y >= 0 && pos.y < chunkSize &&
         pos.z >= 0 && pos.z < chunkSize;
}

int getInd(ivec3 pos){
  int index = pos.x + pos.z * chunkSize + pos.y * chunkSize * chunkSize;
  return (ids[index / 2] >> (index % 2 * 0x10) & 0xffff); //int32 -> int16
}

vec3 intersectionPoint( in vec3 ro, in vec3 rd, in float side, inout float hitDist) 
{
    vec3 m = 1.0/rd;
    float offset = side < 0 ? -1: 0;
    vec3 n = m * (camera.pos_dist.xyz + offset - ro);
    vec3 k = abs(m);
    vec3 t1 = -n - k;
    hitDist = max( max( t1.x, t1.y ), t1.z );
    return camera.pos_dist.xyz + rd * hitDist;
}

float shadowVal(vec3 pos)
{
  return 1;
  vec3 ray = normalize(-light.pos);
  
  ivec3 mapPos = ivec3(floor(pos));

  vec3 deltaDist = abs(vec3(length(ray)) / ray);

  ivec3 rayStep = ivec3(sign(ray));

  vec3 sideDist = (sign(ray) * (vec3(mapPos) - pos) + (sign(ray) * 0.5) + 0.5) * deltaDist; 
  bvec3 mask = lessThanEqual(sideDist.xyz, min(sideDist.yzx, sideDist.zxy));
  sideDist += vec3(mask) * deltaDist;
  mapPos += ivec3(vec3(mask)) * rayStep;
  for (int i = 0; i < camera.pos_dist.w; i++) {
    if (getInd(mapPos) > 0) return 0;
    mask = lessThanEqual(sideDist.xyz, min(sideDist.yzx, sideDist.zxy));
    sideDist += vec3(mask) * deltaDist;
    mapPos += ivec3(vec3(mask)) * rayStep;
  }

  return 1;
}

RayResult rayMarch(vec2 coord){
  RayResult result;
  vec3 ray = normalize(mat3(camera.matrix) * vec3(coord, 1));
  vec3 start = camera.pos_dist.xyz;

  ivec3 mapPos = ivec3(floor(start + 0.));

  vec3 deltaDist = abs(vec3(length(ray)) / ray);

  ivec3 rayStep = ivec3(sign(ray));

  vec3 sideDist = (rayStep * (vec3(mapPos) - start) + (sign(ray) * 0.5) + 0.5) * deltaDist;
  bvec3 mask;
  for (int i = 0; i < camera.pos_dist.w; i++) {
    if ( (result.hit = getInd(mapPos)) > 0 && isValid(mapPos)) break;
    mask = lessThanEqual(sideDist.xyz, min(sideDist.yzx, sideDist.zxy));

    sideDist += vec3(mask) * deltaDist;
    mapPos += ivec3(mask) * rayStep;
  }
  if(!isValid(mapPos)){
    result.hit = 0;
    return result;
  }
  if(result.hit > 0){
    if(mask.x){
      result.normal = vec3(float(-rayStep.x), 0, 0);
      result.pos = intersectionPoint(mapPos, ray, -rayStep.x, result.hitDist);
      result.uv = fract(result.pos.zy);
    }else if(mask.y){
      result.normal = vec3(0, float(-rayStep.y), 0);
      result.pos = intersectionPoint(mapPos, ray, -rayStep.y, result.hitDist);
      result.uv = fract(result.pos.xz);
    }else{
      result.normal = vec3(0, 0, float(-rayStep.z));
      result.pos = intersectionPoint(mapPos, ray, -rayStep.z, result.hitDist);
      result.uv = fract(result.pos.xy);
    }
  }
  return result;
} 

void main()
{
  vec2 uv = vec2(gl_FragCoord.xy - 0.5 * camera.size.xy) / camera.size.y;
  vec3 lightPos = -light.pos.xyz;

  RayResult res = rayMarch(uv);
  if(res.hit > 0){
    float light = dot(res.normal, normalize(lightPos)) * 0.5 + 0.5;
    int hit = res.hit - 1; //Air doesnt have a texture
    float spriteTexel = tileinfo.x * tileinfo.z;
    vec2 uv = vec2(hit % int(tileinfo.y), (hit / int(tileinfo.y))) * spriteTexel + res.uv * spriteTexel;
    vec4 col = texture(tilemap, uv);
    float fog = pow(((res.hitDist / camera.pos_dist.w)), 1.2);
    fragColor = vec4(col.rgb * min(light, shadowVal(floor(res.pos))) + (fog * vec3(0, 0.4, 1)), 1);
    //fragColor = vec4(res.uv, 0, 1);
    //fragColor = vec4(res.hitDist / camera.pos_dist.w);
  }
  else{
    fragColor = vec4(0);
  }
}