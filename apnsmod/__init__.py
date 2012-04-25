from _apnsmod import *
from pypns import PyPNS
from pdspn import PDSPN
from pds import  PDS

algos = {
  'dfpns': (DepthFirstPNS, 'Depth-First Proof-Number Search'),
  'pns': (ProofNumberSearch, 'Proof-Number Search'),
  'pdspn': (PDSPN, 'PDS-PN'),
  'pds': (PDS, 'PDS')
#  'pypns': (PyPNS, 'Python PNS')
}
