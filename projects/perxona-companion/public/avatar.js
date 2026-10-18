import * as THREE from "three";
import { GLTFLoader } from "three/addons/loaders/GLTFLoader.js";
import { VRMLoaderPlugin, VRMUtils } from "@pixiv/three-vrm";

const BONE_NAMES = ["hips", "spine", "chest", "upperChest", "neck", "head", "leftUpperArm", "rightUpperArm", "leftLowerArm", "rightLowerArm"];

export async function createAvatar({ container, modelUrl, onStatus = () => {}, onGestureComplete = () => {} }) {
  const scene = new THREE.Scene();
  const camera = new THREE.PerspectiveCamera(28, 1, 0.05, 100);
  const renderer = new THREE.WebGLRenderer({ alpha: true, antialias: true });
  renderer.setPixelRatio(Math.min(devicePixelRatio, 2));
  renderer.outputColorSpace = THREE.SRGBColorSpace;
  renderer.shadowMap.enabled = true;
  renderer.domElement.className = "avatar-canvas";
  container.replaceChildren(renderer.domElement);
  scene.add(new THREE.HemisphereLight(0xfff7ff, 0x8b7bb3, 2.25));
  const key = new THREE.DirectionalLight(0xffffff, 2.6);
  key.position.set(1.5, 2.5, 2.5); scene.add(key);
  const rim = new THREE.DirectionalLight(0xb6ffe4, 1.2);
  rim.position.set(-2, 1.8, -1); scene.add(rim);

  const loader = new GLTFLoader();
  loader.register((parser) => new VRMLoaderPlugin(parser));
  onStatus("loading");
  const gltf = await loader.loadAsync(modelUrl);
  const vrm = gltf.userData.vrm;
  if (!vrm) throw new Error("VRMモデルを読み込めませんでした。");
  VRMUtils.removeUnnecessaryVertices(gltf.scene);
  VRMUtils.combineSkeletons(gltf.scene);
  VRMUtils.rotateVRM0(vrm);
  scene.add(vrm.scene);

  const box = new THREE.Box3().setFromObject(vrm.scene);
  const size = box.getSize(new THREE.Vector3());
  const center = box.getCenter(new THREE.Vector3());
  vrm.scene.position.x -= center.x;
  vrm.scene.position.y -= box.min.y;
  vrm.scene.position.z -= center.z;
  const height = Math.max(size.y, 1);
  const targetY = height * 0.68;
  camera.position.set(0, targetY, height * 1.35);
  camera.lookAt(0, targetY, 0);

  const bones = Object.fromEntries(BONE_NAMES.map((name) => [name, vrm.humanoid.getNormalizedBoneNode(name)]));
  const base = Object.fromEntries(BONE_NAMES.flatMap((name) => bones[name] ? [[name, bones[name].quaternion.clone()]] : []));
  if (bones.leftUpperArm) bones.leftUpperArm.rotation.z = -1.35;
  if (bones.rightUpperArm) bones.rightUpperArm.rotation.z = 1.35;
  for (const name of ["leftUpperArm", "rightUpperArm"]) if (bones[name]) base[name] = bones[name].quaternion.clone();

  let amplitude = 0, gesture = "idle", gestureStarted = 0, disposed = false, frame = 0;
  const clock = new THREE.Clock();
  function resize() {
    const { clientWidth: width, clientHeight: heightPx } = container;
    if (!width || !heightPx) return;
    renderer.setSize(width, heightPx, false);
    camera.aspect = width / heightPx; camera.updateProjectionMatrix();
  }
  function resetPose() {
    for (const [name, quaternion] of Object.entries(base)) bones[name]?.quaternion.copy(quaternion);
  }
  function animateGesture(time) {
    const elapsed = (time - gestureStarted) / 1000;
    resetPose();
    if (gesture === "nod" && bones.head) bones.head.rotation.x += Math.sin(elapsed * 6) * 0.14;
    if (gesture === "bow") {
      const amount = Math.sin(Math.min(elapsed / 1.3, 1) * Math.PI) * 0.42;
      if (bones.spine) bones.spine.rotation.x += amount;
      if (bones.chest) bones.chest.rotation.x += amount * 0.35;
    }
    if (gesture === "wave" && bones.rightUpperArm) {
      bones.rightUpperArm.rotation.z = 0.18;
      bones.rightUpperArm.rotation.x = 0.12;
      if (bones.rightLowerArm) bones.rightLowerArm.rotation.z = -1.6 + Math.sin(elapsed * 7) * 0.2;
    }
    if (gesture !== "idle" && elapsed > 2.8) {
      gesture = "idle";
      container.dataset.gesture = "idle";
      onGestureComplete();
    }
  }
  function render(time = 0) {
    if (disposed) return;
    frame = requestAnimationFrame(render);
    const delta = Math.min(clock.getDelta(), 0.05);
    animateGesture(time);
    const breath = Math.sin(time / 900) * 0.018;
    if (bones.chest) bones.chest.rotation.x += breath;
    const blinkPhase = time % 4300;
    const blink = blinkPhase > 4050 ? Math.sin(((blinkPhase - 4050) / 250) * Math.PI) : 0;
    vrm.expressionManager?.setValue("blink", blink);
    vrm.expressionManager?.setValue("aa", THREE.MathUtils.lerp(vrm.expressionManager.getValue("aa") || 0, amplitude, 0.35));
    vrm.update(delta);
    renderer.render(scene, camera);
  }
  const observer = new ResizeObserver(resize); observer.observe(container); resize(); render();
  container.dataset.avatarStatus = "loaded";
  onStatus("loaded");
  return {
    setAmplitude(value) { amplitude = THREE.MathUtils.clamp(value, 0, 1); container.dataset.amplitude = amplitude.toFixed(3); },
    playGesture(name) { gesture = ["nod", "wave", "bow"].includes(name) ? name : "nod"; gestureStarted = performance.now(); container.dataset.gesture = gesture; },
    stop() { gesture = "idle"; amplitude = 0; container.dataset.gesture = "idle"; container.dataset.amplitude = "0.000"; },
    dispose() { disposed = true; cancelAnimationFrame(frame); observer.disconnect(); renderer.dispose(); vrm.scene.removeFromParent(); },
  };
}
