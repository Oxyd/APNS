/// \file config.hpp
/// Allows the user to toggle a few compile-time flags.

#ifndef APNSMOD_CONFIG_HPP
#define APNSMOD_CONFIG_HPP

/// If set to 1, vertices will be sorted killers-first after expansion.
#define KILLER_SORT_AFTER_EXPAND 1

/// If set to 1, vertices will be sorted killers-first before selection.
#define KILLER_SORT_BEFORE_SELECT 0

/// If set to 1, killer steps will be tried before an actual vertex expansion.
#define KILLER_SIMULATE 0

/// If set to 1, the simulation using killer steps will continue recursively up to four levels deep. Requires
/// KILLER_SIMULATE = 1.
#define KILLER_SIMULATE_RECURSIVE 0

/// If set to 1, killers will be preferred while selecting a successor.
#define KILLER_PREFER 0

/// If set to 1, initialize PN/DN values for new vertices heuristically.
#define PN_DN_HEURISTIC_INIT 0

// Consistency checks:

#if KILLER_SIMULATE_RECURSIVE == 1 && KILLER_SIMULATE == 0
#   error "KILLER_SIMULATE_RECURSIVE requires KILLER_SIMULATE = 1"
#endif

#endif
