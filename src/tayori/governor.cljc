(ns tayori.governor
  "ComplianceGovernor — the independent censor that earns reply-LLM the right
  to *propose* a draft/revision. The LLM has no notion of consent state,
  redaction requirements, or the no-actuation charter, so this MUST be a
  separate system (rules over the store's ground facts) able to *reject* a
  proposal and fall back to HOLD — the tayori analog of kekkai's
  TailnetGovernor / gijiroku's PrivacyGovernor.

  The actor is **propose → draft/revision only**. It never sends a message
  and never publishes a document; sending/publishing is ALWAYS routed to a
  human (the tayori analog of robotaxi's MRC / itonami's cert hold /
  gijiroku's always-human `:minutes/distribute`). Below, HARD invariants
  force HOLD (a human cannot approve past a consent-blocked recipient, a
  missing redaction, a proposal that claims to have already sent/published,
  or a publish target that doesn't match the document's own registered path);
  a clean send/publish still routes to a human (high-stakes).

  HARD invariants:
    :reply/draft :document/revise
      1. No-actuation — proposal :effect must be :draft/:revision (a
                         control-plane record), never :sent/:published.
      2. Redaction    — every protected-tag cite (:health/:legal/:financial)
                         must appear in :redactions.
    :reply/send
      1. Consent      — the thread's contact must not be :consent :blocked.
    :document/publish
      1. Tenant-isolation — the publish `target` must equal the document's own
                             registered `:path` (no cross-document hijack).
  SOFT:
    3. Confidence floor → escalate.
    4. `:reply/send` and `:document/publish` are high-stakes → ALWAYS human."
  (:require [tayori.policy :as policy]
            [tayori.store :as store]))

(def confidence-floor 0.6)

;; ───────────────────────── invariant checks ─────────────────────────

(defn- actuation-violations [proposal expected]
  (when (not= expected (:effect proposal))
    [{:rule :no-actuation
      :detail (str "この段階の effect は " expected " 固定(propose→"
                   (name expected) " のみ)。実際=" (:effect proposal))}]))

(defn- redaction-violations [proposal]
  (let [missing (policy/missing-redactions (:cites proposal) (:redactions proposal))]
    (when (seq missing)
      [{:rule :missing-redaction :detail (str "機微引用に redaction 無し: " missing)}])))

(defn- consent-violations [st thread-id]
  (let [th (store/thread st thread-id)
        c  (store/contact st (first (:participants th)))]
    (when (policy/consent-blocked? c)
      [{:rule :consent-blocked :detail (str (:id c) " は送信ブロック対象")}])))

(defn- tenant-violations [st document-id target]
  (let [doc (store/document st document-id)]
    (when (policy/target-mismatch? doc target)
      [{:rule :tenant-mismatch
        :detail (str "publish 先 " target " は " document-id " の登録先 " (:path doc) " と不一致")}])))

(defn check
  "Censors a reply-LLM proposal for a tayori op. Returns
   {:ok? :violations :confidence :hard? :escalate? :high-stakes?}.

   Hard violations force HOLD and cannot be overridden. Sending a reply or
   publishing a document is high-stakes → human sign-off even when clean."
  [request proposal st]
  (let [op   (:op request)
        hard (vec (case op
                    :reply/draft
                    (concat (actuation-violations proposal :draft)
                            (redaction-violations proposal))
                    :document/revise
                    (concat (actuation-violations proposal :revision)
                            (redaction-violations proposal))
                    :reply/send
                    (consent-violations st (:thread request))
                    :document/publish
                    (tenant-violations st (:document request) (:target request))
                    []))
        conf    (:confidence proposal 0.0)
        low?    (< conf confidence-floor)
        stakes? (contains? #{:reply/send :document/publish} op)
        hard?   (boolean (seq hard))]
    {:ok?          (and (not hard?) (not low?) (not stakes?))
     :violations   hard
     :confidence   conf
     :hard?        hard?
     :escalate?    (and (not hard?) (or low? stakes?))
     :high-stakes? stakes?}))

(defn hold-fact [request verdict]
  {:t :tayori-hold :op (:op request) :subject (or (:thread request) (:document request))
   :disposition :hold :basis (mapv :rule (:violations verdict))
   :violations (:violations verdict) :confidence (:confidence verdict)})
