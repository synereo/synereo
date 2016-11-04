package com.synereo.wallet.models

case class BalanceSummary(address: String, amp: String, btc: String, errors: List[String]) extends Serializable
