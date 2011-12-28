.First.lib <- function(lib,pkg) {
	#binaries list:
	.binlist <- c('addnoise', 'd2', 'lyap_k', 'polynomp', 'spectrum','ar-model', 'delay', 'lyap_r', 'polypar', 'spikeauto', 'ar-run', 'endtoend', 'lyap_spec', 'predict', 'spikespec','autocor', 'events', 'makenoise', 'project', 'stp', 'av-d2', 'extrema', 'mem_spec', 'randomize_auto_exp_random', 'surrogates', 'boxcount', 'false_nearest', 'mutual', 'randomize_autop_exp_random', 'svd', 'c1', 'fsle', 'notch', 'randomize_spikeauto_exp_random', 'timerev', 'c2d', 'ghkss', 'nrlazy', 'randomize_spikespec_exp_event', 'upo', 'c2g', 'henon', 'nstat_z', 'randomize_uneven_exp_random', 'upoembed', 'c2naive', 'histogram', 'nstep', 'rbf', 'wiener1', 'c2t', 'ikeda', 'onestep', 'recurr', 'wiener2', 'choose', 'intervals', 'pc', 'resample', 'xc2', 'cluster', 'lazy', 'poincare', 'rescale', 'xcor', 'compare', 'll-ar', 'polyback', 'rms', 'xzero', 'corr', 'low121', 'polynom', 'sav_gol', 'zeroth')
	assign(".binlist", .binlist, env=as.environment("package:RTisean"))

        packageStartupMessage("RTisean loaded and initialized\n")
}


