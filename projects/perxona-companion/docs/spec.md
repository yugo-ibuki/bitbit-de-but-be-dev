# Luma Local Avatar Companion

## Goal

Macのブラウザから日本語で会話し、ローカルVRMアバターがCodexの返答を端末内音声で読み上げて動く。

## Architecture

- Node.jsのlocalhost HTTPサーバー。会話はChatGPTログイン済み`codex exec`。
- Three.js + `@pixiv/three-vrm`をesbuildでself-hostする。VRMモデルもローカル配信する。
- rendererは`createAvatar({ container, modelUrl, onStatus })`としてUIから分離し、振幅、gesture、stop、disposeを公開する。
- macOS `say`へ本文をstdinで渡して一時WAVを生成する。ブラウザはWeb Audio analyserの振幅で口形`aa`を動かす。
- 自然なblink、breath、neutral armと、nod、wave、bowをnormalized humanoid bonesで実装する。
- 発話生成中のfetchと再生中AudioBufferSourceを同じ停止操作で中断し、口とposeをneutralへ戻す。

## Acceptance

1. `npm ci && npm start`でbundle生成を含めて再現できる。
2. Codex実応答、複数turn、入力検証、失敗後再送が動く。
3. VRMの頭部と上半身が見え、blink、breath、lip sync、gestureが動く。
4. 音声生成はinstalled voiceだけ、最大長、timeout、single-flight、tmp cleanupを満たす。
5. 外部CDN、Perxona runtime、Web Speech認識・合成を使わない。
6. 127.0.0.1 listen、Host/Origin、限定静的route、self中心CSPを維持する。
7. mobile縦積み、IME、aria-live、pending、error、stopを維持する。

## Non-goals

Flutter実装、BehaviorAI、公開hosting、多人数認証、クラウドTTS、音声入力、課金・購入。
