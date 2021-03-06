Background
----------

Choose Energy is an online platform for choosing an electricity or
natural gas supplier. In 19 states in America utilities are deregulated
so that you must choose an electricity and/or natural gas plan for your
home. Choose Energy's site provides an easy comparisons of prices, term
lengths, contract types and more to educate consumers on how they can
choose the best energy plan for them and hopefully save some money in
the process.

The site's inventory of electricity and natural gas plans is open to
many individual suppliers, who automate changes to their plans on a
daily basis. The inventory is large and wide-spread across different
states and products, so this app helps to determine what changes are
being made, which are the most important to look at, and which actually
have an impact on conversions on the site.

How It Works
============

After plan changes occur, the algorithm goes through and detects them
and marks the date of occurrence. It then looks to see if there was any
impact on lead volume dating back to this specific instance. If an
impact is statistically significant, it marks it with a level of
importance based on the amount of sales volume that was impacted. It
then sorts to find the most critical ones in the past 40 days and
cross-checks the impacts with conversion rate to see if the impact was
just in lead volume or also conversion rate.

If conversion rate goes down or up relative to itself within the 95%
confidence interval that lead volume goes down or up then it looks back
at all the plan changes around that date once again. From here it looks
at the top contributing plans to sales volume in that utility zone and
finds the top correlating plans to overall sales volume. If one has a
higher correlation than the rest, it looks back to all plan changes for
that plan if a plan change falls within the specific date range of the
impact, it attributes the impact specifically to the change in that plan
on that date and specifies the change.

If it is clear that multiple plans and/or plan changes occurred that
correlated highly with the impact, it lists all of those highly
correlating plans so that one can dig deeper to determine which of them
might be relevant to be aware of. Otherwise, if conversion rate remains
relatively the same or goes up while lead volume decreases, it looks
back at the traffic for that utility zone and determines that the impact
was mostly caused by changes in traffic to the site in that utility
zone.
