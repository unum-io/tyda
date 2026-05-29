package com.choreograph.tyda

/** A big named tuple for validating compiletime performance of typeclass
  * derivation
  *
  * We use an opaque type just to make the companion object part of given scope.
  */
opaque type BigNamedTuple >: BigNamedTuple.Repr <: BigNamedTuple.Repr = BigNamedTuple.Repr

object BigNamedTuple {
  // format: off
  type Repr = (
    i0: Int, i1: Int, i2: Int, i3: Int, i4: Int, i5: Int, i6: Int, i7: Int, i8: Int, i9: Int, i10: Int,
    i11: Int, i12: Int, i13: Int, i14: Int, i15: Int, i16: Int, i17: Int, i18: Int, i19: Int, i20: Int,
    i21: Int, i22: Int, i23: Int, i24: Int, i25: Int, i26: Int, i27: Int, i28: Int, i29: Int, i30: Int,
    i31: Int, i32: Int, i33: Int, i34: Int, i35: Int, i36: Int, i37: Int, i38: Int, i39: Int, i40: Int,
    i41: Int, i42: Int, i43: Int, i44: Int, i45: Int, i46: Int, i47: Int, i48: Int, i49: Int, i50: Int,
    i51: Int, i52: Int, i53: Int, i54: Int, i55: Int, i56: Int, i57: Int, i58: Int, i59: Int, i60: Int,
    i61: Int, i62: Int, i63: Int, i64: Int, i65: Int, i66: Int, i67: Int, i68: Int, i69: Int, i70: Int,
    i71: Int, i72: Int, i73: Int, i74: Int, i75: Int, i76: Int, i77: Int, i78: Int, i79: Int, i80: Int,
    i81: Int, i82: Int, i83: Int, i84: Int, i85: Int, i86: Int, i87: Int, i88: Int, i89: Int, i90: Int,
    i91: Int, i92: Int, i93: Int, i94: Int, i95: Int, i96: Int, i97: Int, i98: Int, i99: Int, i100: Int,
    i101: Int, i102: Int, i103: Int, i104: Int, i105: Int, i106: Int, i107: Int, i108: Int, i109: Int, i110: Int,
    i111: Int, i112: Int, i113: Int, i114: Int, i115: Int, i116: Int, i117: Int, i118: Int, i119: Int, i120: Int,
    i121: Int, i122: Int, i123: Int, i124: Int, i125: Int, i126: Int, i127: Int, i128: Int, i129: Int, i130: Int,
    i131: Int, i132: Int, i133: Int, i134: Int, i135: Int, i136: Int, i137: Int, i138: Int, i139: Int, i140: Int,
    i141: Int, i142: Int, i143: Int, i144: Int, i145: Int, i146: Int, i147: Int, i148: Int, i149: Int, i150: Int,
  )
  // format: on

  given Arbitrary[BigNamedTuple] =
    for arr <- Arbitrary.seqN[Int](valueOf[NamedTuple.Size[BigNamedTuple]])
    yield
      // TYPE SAFETY: NamedTuples are erased to regular tuples at runtime
      Tuple.fromArray(arr.toArray).asInstanceOf[BigNamedTuple]
}
