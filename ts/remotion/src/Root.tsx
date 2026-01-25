import { Composition } from "remotion";
import { HelloWorld } from "./HelloWorld";
import { AdvancedVideo } from "./AdvancedVideo";
import { StarWarsIntro } from "./StarWarsIntro";
import { GitTutorial } from "./GitTutorial";

export const RemotionRoot: React.FC = () => {
  return (
    <>
      {/* シンプルなサンプル */}
      <Composition
        id="HelloWorld"
        component={HelloWorld}
        durationInFrames={150}
        fps={30}
        width={1920}
        height={1080}
        defaultProps={{
          titleText: "Hello, Remotion!",
          titleColor: "#ffffff",
        }}
      />

      {/* 凝ったサンプル（トランジション付き複数シーン） */}
      <Composition
        id="AdvancedDemo"
        component={AdvancedVideo}
        durationInFrames={450}
        fps={30}
        width={1920}
        height={1080}
      />

      {/* dot-claude-sync紹介（スターウォーズ風） */}
      <Composition
        id="DotClaudeSync"
        component={StarWarsIntro}
        durationInFrames={900}
        fps={30}
        width={1920}
        height={1080}
      />

      {/* Git & GitHub 入門 */}
      <Composition
        id="GitTutorial"
        component={GitTutorial}
        durationInFrames={1100}
        fps={30}
        width={1920}
        height={1080}
      />
    </>
  );
};
