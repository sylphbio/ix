(module ix ()

(import chicken.module)
(reexport (prefix (except ix.base prototype) ix:))
(reexport (prefix ix.build ix:))
(reexport (prefix (only ix.static tags prototype-tags object-tags complex-tags primitive-tags) ix:))
(reexport (prefix ix.parse parse:))
(reexport (prefix ix.stringify stringify:))
(reexport ix.lens)

)
