import { Grid, Line, Sparkles, Text } from '@react-three/drei'
import { useMemo } from 'react'

interface MeasurementGuideProps {
  height: number
}

export function MeasurementGuide({ height }: MeasurementGuideProps) {
  const rulerHeight = Math.max(8, Math.ceil(height / 2) * 2)
  const ticks = useMemo(
    () => Array.from({ length: rulerHeight / 2 + 1 }, (_, index) => index * 2),
    [rulerHeight],
  )

  return (
    <>
      <Grid
        args={[30, 30]}
        position={[0, -0.01, 0]}
        cellSize={1}
        cellThickness={0.35}
        cellColor="#164052"
        sectionSize={5}
        sectionThickness={0.8}
        sectionColor="#42cfe8"
        fadeDistance={24}
        fadeStrength={1.2}
        infiniteGrid
      />
      <Sparkles
        count={36}
        scale={[15, 8, 15]}
        color="#69e6ff"
        size={1.4}
        speed={0.18}
        opacity={0.32}
      />

      <Line
        points={[
          [6.2, 0, 0],
          [6.2, rulerHeight, 0],
        ]}
        color="#69e6ff"
        transparent
        opacity={0.72}
        lineWidth={1}
      />
      {ticks.map((tick) => (
        <group key={tick} position={[6.2, tick, 0]}>
          <Line
            points={[
              [0, 0, 0],
              [-0.28, 0, 0],
            ]}
            color="#69e6ff"
            transparent
            opacity={0.78}
            lineWidth={1}
          />
          <Text
            position={[0.42, 0, 0]}
            fontSize={0.22}
            color="#9addeb"
            anchorX="left"
            anchorY="middle"
          >
            {tick}m
          </Text>
        </group>
      ))}

      {height > 0 && (
        <>
          <Line
            points={[
              [-4.2, height, 0],
              [6.2, height, 0],
            ]}
            color="#b9ff69"
            transparent
            opacity={0.74}
            dashed
            dashSize={0.18}
            gapSize={0.12}
            lineWidth={1}
          />
          <Text
            position={[5.95, height + 0.24, 0]}
            fontSize={0.22}
            color="#b9ff69"
            anchorX="right"
            anchorY="bottom"
          >
            {height.toFixed(2)}m
          </Text>
        </>
      )}
    </>
  )
}
