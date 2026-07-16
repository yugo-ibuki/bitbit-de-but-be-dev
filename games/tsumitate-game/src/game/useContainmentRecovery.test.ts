import { act, renderHook } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { useContainmentRecovery } from './useContainmentRecovery'

describe('useContainmentRecovery', () => {
  beforeEach(() => vi.useFakeTimers())
  afterEach(() => vi.useRealTimers())

  it('restores three seconds after the latest destruction', () => {
    const restore = vi.fn()
    const { rerender } = renderHook(
      ({ version }) => useContainmentRecovery(false, version, restore),
      { initialProps: { version: 1 } },
    )

    act(() => vi.advanceTimersByTime(2000))
    rerender({ version: 2 })
    act(() => vi.advanceTimersByTime(2999))
    expect(restore).not.toHaveBeenCalled()
    act(() => vi.advanceTimersByTime(1))
    expect(restore).toHaveBeenCalledOnce()
  })

  it('cancels recovery when containment is already enabled', () => {
    const restore = vi.fn()
    const { rerender } = renderHook(
      ({ enabled }) => useContainmentRecovery(enabled, 1, restore),
      { initialProps: { enabled: false } },
    )

    rerender({ enabled: true })
    act(() => vi.advanceTimersByTime(3000))
    expect(restore).not.toHaveBeenCalled()
  })
})
