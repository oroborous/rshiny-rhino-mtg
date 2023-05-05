-- View: public.v_user_cards

-- DROP VIEW public.v_user_cards cascade;

CREATE OR REPLACE VIEW public.v_user_cards
 AS
 SELECT d.useremail,
    b.setcode,
    c.name AS setname,
    c.releasedate,
    b.uuid,
    b.title AS cardname,
    b.type,
    b.color,
    round(b.avgretailprice, 2) AS avgretailprice,
	round(b.avgbuylistprice, 2) AS avgbuylistprice,
    d.numowned
   FROM mtg_collections d
     JOIN mtg_cards b ON d.uuid = b.uuid
     JOIN mtg_sets c ON b.setcode = c.code;

ALTER TABLE public.v_user_cards
    OWNER TO doadmin;

-- View: public.v_set_cards

-- DROP VIEW public.v_set_cards cascade;

CREATE OR REPLACE VIEW public.v_set_cards
 AS
 SELECT c.code AS setcode,
    c.name AS setname,
    c.releasedate,
    b.uuid,
    b.title AS cardname,
    b.type,
    b.color,
    round(b.avgretailprice, 2) AS avgretailprice,
	round(b.avgbuylistprice, 2) as avgbuylistprice,
    c.totalsetsize
   FROM mtg_sets c
     JOIN mtg_cards b ON b.setcode = c.code;

ALTER TABLE public.v_set_cards
    OWNER TO doadmin;

