import { useEffect } from 'react'

export const CONTAINMENT_RECOVERY_MS = 3000

export function useContainmentRecovery(
  containmentEnabled: boolean,
  destructionVersion: number,
  onRestore: () => void,
): void {
  useEffect(() => {
    if (containmentEnabled || destructionVersion === 0) return
    const timer = window.setTimeout(onRestore, CONTAINMENT_RECOVERY_MS)
    return () => window.clearTimeout(timer)
  }, [containmentEnabled, destructionVersion, onRestore])
}
