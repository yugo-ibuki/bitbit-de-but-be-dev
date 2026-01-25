import {
  linearTiming,
  springTiming,
  TransitionSeries,
} from "@remotion/transitions"
import { fade } from "@remotion/transitions/fade"
import { slide } from "@remotion/transitions/slide"
import { wipe } from "@remotion/transitions/wipe"

import { IntroScene } from "./scenes/IntroScene"
import { FeatureScene } from "./scenes/FeatureScene"
import { CodeScene } from "./scenes/CodeScene"
import { OutroScene } from "./scenes/OutroScene"

export const AdvancedVideo: React.FC = () => {
  return (
    <TransitionSeries>
      {/* イントロ */}
      <TransitionSeries.Sequence durationInFrames={90}>
        <IntroScene />
      </TransitionSeries.Sequence>

      {/* Fade トランジション */}
      <TransitionSeries.Transition
        timing={springTiming({ config: { damping: 200 } })}
        presentation={fade()}
      />

      {/* 特徴紹介 */}
      <TransitionSeries.Sequence durationInFrames={120}>
        <FeatureScene />
      </TransitionSeries.Sequence>

      {/* Slide トランジション */}
      <TransitionSeries.Transition
        timing={linearTiming({ durationInFrames: 20 })}
        presentation={slide({ direction: "from-right" })}
      />

      {/* コードデモ */}
      <TransitionSeries.Sequence durationInFrames={120}>
        <CodeScene />
      </TransitionSeries.Sequence>

      {/* Wipe トランジション */}
      <TransitionSeries.Transition
        timing={linearTiming({ durationInFrames: 25 })}
        presentation={wipe({ direction: "from-bottom-left" })}
      />

      {/* アウトロ */}
      <TransitionSeries.Sequence durationInFrames={120}>
        <OutroScene />
      </TransitionSeries.Sequence>
    </TransitionSeries>
  )
}
