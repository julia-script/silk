import * as Target from '../dist/Target.js'

// Documentation selects the same logical profile facts as source analysis. Hosted and raw Linux
// declarations can differ even when their target triple is identical.
export const documentationProfiles = Object.freeze(
  Target.all.flatMap((target) => [
    { name: target.id, profile: { target: target.id } },
    ...(target.operatingSystem === 'linux'
      ? [
          {
            name: `${target.id}-no-libc`,
            profile: { target: target.id, libc: 'none' },
          },
        ]
      : []),
  ]),
)
