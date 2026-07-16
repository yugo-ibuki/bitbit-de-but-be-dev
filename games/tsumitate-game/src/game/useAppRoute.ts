import { useCallback, useEffect, useState } from 'react'

export type AppRoute = 'game' | 'ranking'

const routePath: Record<AppRoute, string> = {
  game: '/',
  ranking: '/ranking',
}

export function getRoute(pathname: string): AppRoute {
  return pathname === routePath.ranking ? 'ranking' : 'game'
}

export function useAppRoute() {
  const [route, setRoute] = useState<AppRoute>(() =>
    typeof window === 'undefined' ? 'game' : getRoute(window.location.pathname),
  )

  useEffect(() => {
    const syncRoute = () => setRoute(getRoute(window.location.pathname))
    window.addEventListener('popstate', syncRoute)
    return () => window.removeEventListener('popstate', syncRoute)
  }, [])

  const navigate = useCallback((nextRoute: AppRoute) => {
    const pathname = routePath[nextRoute]
    if (window.location.pathname !== pathname) {
      window.history.pushState({}, '', pathname)
    }
    setRoute(nextRoute)
  }, [])

  return { route, navigate }
}
